-module(p2p_tcp_node).
-export([tcp_listener/1, handle_client/1]).

tcp_listener(ListenSocket) ->

    % TCP_PORT = 12345,
    % {ok, ListenSocket} = gen_tcp:listen(TCP_PORT, [{active, false}, {packet, line}]),

    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Socket accepted~n"),
    spawn(?MODULE, handle_client, [Socket]),
    tcp_listener(ListenSocket).

handle_client(Socket) ->
    io:format("Listening~n"),
    % inet:setopts(Socket, [{active, once}]),
    % io:format("Active once~n"),
    % receive
    %     {tcp, Socket, <<"listar_archivos", _/binary>>} ->
    %         {ok, Filenames} = file:list_dir("./compartida"),
    %         io:format("Archivos: ~n~w~n",Filenames),
    %         gen_tcp:send(Socket, Filenames),
    %         handle_client(Socket);
    %     {tcp, Socket, <<"quit", _/binary>>} ->
    %         gen_tcp:close(Socket);
    %     {tcp, Socket, Msg} ->
    %         io:format("Mensaje de cliente: ~w~n",[Msg]),
    %         gen_tcp:send(Socket, Msg),
    %         handle_client(Socket);
    %     Other -> io:format("Received ~w~n", [Other])
    % end.

    case gen_tcp:recv(Socket, 0) of

        {ok, <<"SEARCH_REQUEST ", IdBin:32/bitstring, " ", SearchTermN/binary>>} ->
            Id = binary_to_list(IdBin),
            [SearchTerm, <<>>] = binary:split(SearchTermN, <<"\n">>),
            io:format("Recibido: ~s from ~s~n", [SearchTerm, Id]),

            % Búsqueda

            % Respuesta
            % gen_tcp:send(Socket, <<"SEARCH_RESPONSE ",  IdBin/binary, " ", Filename/binary, " ", SizeBin/binary, "\n">>),

            gen_tcp:send(Socket, <<"Mensaje recibido\n">>),
            handle_client(Socket);

        {ok, Data} ->
            io:format("Recibido: ~p~n", [Data]),
            gen_tcp:send(Socket, <<"Mensaje recibido\n">>),
            handle_client(Socket);
        {error, closed} ->
            io:format("Cliente desconectado~n"),
            gen_tcp:close(Socket);
        _ -> handle_client(Socket)
    end.