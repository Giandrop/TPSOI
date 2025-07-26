-module(p2p_node).
-import(p2p_utils, [generate_ID/0]).
-import(p2p_udp, [id_requester/3, udp_listener/2, udp_announcement/2]).
-export([init_node/0]).

init_node( ) -> 
    %[Obtener id unico]
    % Aca va la logica para obtener un id unico.
    NewId = generate_ID(),

    %[Escanear carpeta compartida]
    file:make_dir("./share_files"),
    {ok, ListFiles} = file:list_dir_all("./share_files"),
    ListFiles,

    %[Inicializo archivo de nodos conocidos]
    Pid_RW = file:open("./know_nodes.json", [write]),
    
    %---------------------------------------------------------------------------------------------
    UDP_PORT = 12346,
    TCP_PORT = 12345,
    
    {ok, Socket} = gen_udp:open( UDP_PORT, [binary, {active, false}, {broadcast, true}]),

    Pid_id_manager   = spawn(p2p_udp, id_manager, [no_id, []]),
    register(id_manager, Pid_id_manager),
    Pid_id_requester = spawn(p2p_udp, id_requester, [UDP_PORT, Socket]),

    Pid_udp_listener = spawn(p2p_udp, udp_listener, [Socket, Pid_id_requester]),
    % Pid_tcp_listener = spawn(p2p_tcp, tcp_listener, []),

    %Proceso encargado de enviar los HELLO de forma periodica.
    Pid_udp_announcement = spawn(p2p_udp, udp_announcement, [Socket, UDP_PORT, TCP_PORT]).

    %---------------------------------------------------------------------------------------------
    %Conexiones via TCP

    %---------------------------------------------------------------------------------------------
