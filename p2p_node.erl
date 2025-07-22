-module(p2p_node).
-export([init_node/0]).

generate_ID() ->
    1.

% Proceso que se encarga de enviar un id y esperar un invalid_name o timeout
id_requester(Port, Socket, Try_id) ->
    BroadcastAddr = {255,255,255,255},
    gen_udp:send(Socket, BroadcastAddr, Port, <<"NAME_REQUEST ", Try_id>>),

% Se queda escuchando durante 45 segundos si algun otro proceso le dice algo
    receive
        {id_fail, Fail_Id} ->
            %Mostrar en pantalla que cual Id fallo
            NewId = generate_ID(),
            id_requester(Port, Socket, NewId)
    after
        45000 -> %setear id 
            Try_id %Ver como pasar esto
    end.

udp_listener(Socket, Broad_PID) ->
    case gen_udp:recv(Socket) of
        {NAME_REQUEST, Pretended_Id} ->
            %Confirmarte si es valido
            % Tengo dos opciones:
            %   -Si el ID no es valido, le mando {invalid_name, Pretended_Id}.
            %   -No le mando nada.
            udp_listener(Socket, Broad_PID),
            true;
        {HELLO, Id, Ip, PORT} ->
            %   Almaceno info en know_nodes.json
            udp_listener(Socket, Broad_PID),
            true;
        {invalid_name, Fail_Id} ->
            
            Broad_PID ! {id_fail, Fail_Id},
        
            udp_listener(Socket,Broad_PID),
            true
    end.

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
    
    %Socket Connect via UDP
    PORT = 12345,
    Socket = gen_udp:open( PORT, [{broadcast, true}]),

    Pid_Broad = spawn(p2p_node, id_requester, [PORT, Socket, NewId]),

    Pid_UDP_Listener = spawn(p2p_node, udp_listener, [Socket, Pid_Broad]).

    
%TODO
% Comunicación TCP con la red
% Ver como devolver que el Id pretendido es valido
% Ver como generar Ids
% Como escribir en .json con erlang