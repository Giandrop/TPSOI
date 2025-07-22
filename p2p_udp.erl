-module(p2p_udp).
-import(p2p_utils, [generate_ID/0]).
-export([id_requester/3, udp_listener/2, udp_announcement/3]).


% Proceso que se encarga de enviar un id y esperar un invalid_name o timeout
id_requester(Port, Socket, BroadcastAddr) ->
    Try_id = generate_ID(),
    gen_udp:send(Socket, BroadcastAddr, Port, <<"NAME_REQUEST ", Try_id>>),

% Se queda escuchando durante 45 segundos si algun otro proceso le dice algo
    receive
        {id_fail, Fail_Id} ->
            %Mostrar en pantalla que cual Id fallo
            id_requester(Port, Socket, BroadcastAddr)
    after
        45000 -> %setear id 
            Try_id %Ver como pasar esto
    end.


%----------------------------------------------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------------------


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


%----------------------------------------------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------------------


udp_announcement(Socket, UDP_PORT, TCP_PORT) ->

    receive
        {ok, Id} -> 
            erlang:send_after(20000, self(), {ok, Id}),
            Msg = "HELLO " ++ Id ++ " " ++ integer_to_list(TCP_PORT) ++ "\n",
            gen_udp:send(Socket, {255, 255, 255, 255}, UDP_PORT, Msg),
            udp_announcement(Socket, UDP_PORT, TCP_PORT)
    end.
