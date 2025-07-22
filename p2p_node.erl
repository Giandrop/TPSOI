-module(p2p_node).
-imUDP_PORT(p2p_utils, [generate_ID/0]).
-imUDP_PORT(p2p_udp, [id_requester/3, udp_listener/2, udp_announcement/2]).
-exUDP_PORT([init_node/0]).

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
    
    BroadcastAddr = {255,255,255,255},
    Socket = gen_udp:open( UDP_PORT, [{broadcast, true}]),

    Pid_id_requester = spawn(p2p_udp, id_requester, [UDP_PORT, Socket, BroadcastAddr]),

    Pid_udp_listener = spawn(p2p_udp, udp_listener, [Socket, Pid_id_requester]),
    Pid_tcp_listener = spawn(p2p_tcp, tcp_listener, []),

    %Proceso encargado de enviar los HELLO de forma periodica.
    Pid_udp_announcement = spawn(p2p_udp, udp_announcement, [Socket, UDP_PORT, TCP_PORT]).

    %---------------------------------------------------------------------------------------------
    %Conexiones via TCP

    %---------------------------------------------------------------------------------------------

    
%TODO
% Comunicación TCP con la red
% Ver como devolver que el Id pretendido es valido
% Ver como generar Ids
% Como escribir en .json con erlang