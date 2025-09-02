-module(p2p_tcp_client).
-export([tcp_client/0, tcp_connection/6]).

tcp_connection(NodeId, Addr, Port, MyId, SearchTerm, SupPid) ->
    % Por cada uno:
    io:format("Conectando a ~w:~p~n", [Addr, Port]),
    {ok, Socket} = gen_tcp:connect(Addr, Port, [binary, {active, false}, {packet, line}]),
    io:format("Conectado ~n"),
    MyIdBin = list_to_binary(MyId), %Ojo: Id puede ser no_id
    SearchTermBin = list_to_binary(SearchTerm),
    io:format("Enviando ~s ~s ~n", [MyId, SearchTerm]),
    gen_tcp:send(Socket, <<"SEARCH_REQUEST ",  MyIdBin/binary, " ", SearchTermBin/binary, "\n">>),

    % Supongo que el timeout lo hacemos acá, para que el proceso no quede colgado indefinidamente
    % si un nodo no responde.
    Timeout = 10000,
    case gen_tcp:recv(Socket, 0, Timeout) of
        % En teoría, ResponseId debería ser igual que NodeId
        % No sé en que situación podrían ser diferentes... 
        % De momento, nos quedamos con el Id que llega en la respuesta
        {ok, <<"SEARCH_RESPONSE ",  ResponseIdBin:32/bitstring, " ", FilenameAndSizeN/binary>>} -> 
            ResponseId = binary_to_list(ResponseIdBin),
            % Extraer Filename y tamaño
            [FilenameAndSize, <<>>] = binary:split(FilenameAndSizeN, <<"\n">>),
            [FilenameBin, SizeBin] = binary:split(FilenameAndSize, <<" ">>),
            Filename = binary_to_list(FilenameBin),
            Size = binary_to_integer(SizeBin),
            % io:format("Recibido: ~s from ~s~n", [SearchTerm, Id]),
            Result = {ResponseId, Filename, Size};
            % Devolver como tupla;
        {error, timeout} -> Result = {NodeId, no_response};
        {error, Reason} -> Result = {NodeId, no_response}
    end,
    io:format("Enviando respuesta ~n", []),
    SupPid ! {self(), Result}.


tcp_client() ->
    % En Erlang: Lanzar procesos para cada consulta a un nodo remoto. Recopilar las respuestas
    % mediante receive con after timeout en el proceso de la CLI o en un proceso coordinador,
    % aprovechando la naturaleza asíncrona de los mensajes.

    % Creo que conviene hacer que la CLI spawnee un proceso con esta función en lugar cada vez
    % que se haga una consulta. Para poder hacer más de una consulta al mismo tiempo? Tiene sentido?
    % Hmmm me parece que no. ¿Hay alguna otra funcionalidad además de search?

    receive
        {search, SearchTerm} ->
            io:format("Got: ~s~n", [SearchTerm]),
            % Obtener la lista de nodos conocidos
            whereis(registry_id) ! {get_list, self()},
            receive {Known_nodes} -> ok end,
            % Known_nodes tiene la forma [{Id, {Addr, Port}}]
            io:format("Known nodes: ~w~n", [Known_nodes]),

            whereis(id_manager) ! {self(), get_id},
            receive {Pid_manager, MyId} -> ok end,
            io:format("Mi id: ~w~n", [MyId]),

            SupPid = self(),
            io:format("Mi pid: ~w~n", [SupPid]),

            % Pids = [ spawn_link() || _ <- lists:seq(1,20) ],
            Pids = lists:map(fun ({NodeId, {Addr, Port}}) -> 
                spawn_link(?MODULE, tcp_connection, [NodeId, Addr, Port, MyId, SearchTerm, SupPid]) end , Known_nodes),
            Results = [receive {Pid, Result} -> Result end || Pid <- Pids],

            io:format("Results: ~w~n", [Results]);

            % tcp_client(); % el tcp_client debería ser efímero

            % handle_client(Socket);
            % {error, closed} ->
            %     io:format("Cliente desconectado~n"),
            %     gen_tcp:close(Socket);
        _ -> ok %tcp_client()
    end.