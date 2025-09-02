-module(p2p_node).
-import(p2p_utils, [generate_ID/0]).
-import(p2p_udp, [id_requester/3, udp_listener/2, udp_announcement/2]).
-export([init_node/0, registry/1, read_registry/0, write_registry/1, file_proc/0]).

-define(REGISTRY, "./know_nodes.txt").

% Nodos conocidos: mantenemos un mapa en memoria
% Esto nos permite evitar estar constantemente leyendo y escribiendo a un archivo
% Tenemos una función read_registry para construir el mapa a partir del archivo de
% registro y una función write_registry para guardarlo

read_registry() ->
    {ok, File} = file:open(?REGISTRY, [read]),
    read_line(File, #{}).

read_line(File, Map) -> 
    case io:fread(File, "", "~4s ~d.~d.~d.~d ~d\n") of
        {ok, [Id, Ip1, Ip2, Ip3, Ip4, Port]} -> 
            NewMap = maps:put(Id, {{Ip1, Ip2, Ip3, Ip4}, Port}, Map),
            read_line(File, NewMap);
        eof -> 
            file:close(File),
            Map
    end.

write_registry(Map) ->
    {ok, File} = file:open(?REGISTRY, [write]),
    write_line(File, maps:to_list(Map)).

write_line(File, Lines) -> 
    case Lines of
        [] -> file:close(File);
        [{Id, {{Ip1,Ip2,Ip3,Ip4}, Port}} | Rest] ->
            io:fwrite(File, "~s ~w.~w.~w.~w ~w~n", [Id, Ip1,Ip2,Ip3,Ip4, Port]),
            write_line(File, Rest)
    end.

registry(Map) ->
    io:format("START REGISTRY ~w ~n", [Map]),
    receive
        {update, Id, Addr, Port} ->
            io:format("GOT UPDATE ~s ~w ~w ~n", [Id, Addr, Port]),
            NewMap = maps:put(Id, {Addr, Port}, Map),
            write_registry(NewMap),
            registry(NewMap);
        {get_list, Pid} ->
            Pid ! {maps:to_list(Map)},
            registry(Map);
        _ -> registry(Map)
    end.

file_proc() ->
    receive 
        {PID, list_dir_all} -> 
            case file:list_dir_all("./share_files") of
                {ok, ListFiles} -> PID ! {self(), ListFiles}
            end
    end,
    file_proc().


init_node( ) -> 
    %[Obtener id unico]
    % Aca va la logica para obtener un id unico.
    NewId = generate_ID(),

    %[Escanear carpeta compartida]
    file:make_dir("./share_files"),
    {ok, ListFiles} = file:list_dir_all("./share_files"),
    ListFiles,

    %[Inicializo archivo de nodos conocidos]
    Pid_RW = file:open(?REGISTRY, [write]),
    Registry = read_registry(),
    io:format("REGISTRY: ~w ~n", [Registry]),
    Registry_id = spawn(?MODULE, registry, [Registry]),
    register(registry_id, Registry_id),
    
    %---------------------------------------------------------------------------------------------
    UDP_PORT = 12346,
    TCP_PORT = 12345,
    
    {ok, Socket} = gen_udp:open( UDP_PORT, [binary, {active, false}, {broadcast, true}]),

    File_proc_id = spawn(?MODULE, file_proc, []),
    register(file_proc, File_proc_id),

    Pid_id_manager   = spawn(p2p_udp, id_manager, [no_id, []]),
    register(id_manager, Pid_id_manager),
    Pid_id_requester = spawn(p2p_udp, id_requester, [UDP_PORT, Socket]),

    Pid_udp_listener = spawn(p2p_udp, udp_listener, [Socket, Pid_id_requester]),
    % Pid_tcp_listener = spawn(p2p_tcp, tcp_listener, []),

    %Proceso encargado de enviar los HELLO de forma periodica.
    Pid_udp_announcement = spawn(p2p_udp, udp_announcement, [Socket, UDP_PORT, TCP_PORT]),

    %---------------------------------------------------------------------------------------------
    %Conexiones via TCP

    %---------------------------------------------------------------------------------------------
    Pid_tcp_supervisor = spawn(p2p_tcp_supervisor, tcp_supervisor, [TCP_PORT]).
    % spawnea un supervisor que crea el socket de escucha y 
    % a su vez spawnea procesos que aceptan conexiones y se la pasa a un proceso que maneja
    % los mensajes. No es exactamente como dice en el enunciado pero creo que con un supervisor
    % es más robusto. Lo vemos

    % Pid_tcp_client = spawn(p2p_tcp_client, tcp_client, []),
    % register(tcp_client, Pid_tcp_client).
