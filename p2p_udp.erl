-module(p2p_udp).
-import(p2p_utils, [generate_ID/0]).
-export([id_manager/2, id_requester/4, udp_listener/3, udp_announcement/3]).


id_manager(Id, Tried_Ids) ->
    receive
        {Pid, get_id} -> 
            Pid ! {self(), Id},
            id_manager(Id, Tried_Ids);
        {Pid, get_tried_ids} ->
            Pid ! {self(), Tried_Ids},
            id_manager(Id, Tried_Ids);
        {set_id, NewId} -> 
            id_manager(NewId, Tried_Ids);
        {try_id, Try_id} -> 
            id_manager(Id, [Try_id | Tried_Ids]);
        {name_request, RequestedId, Socket, Port, Addr} ->
            case RequestedId =:= Id or lists:member(RequestedId, Tried_Ids) of
                true ->
                    gen_udp:send(Socket, Port, Addr, <<"INVALID_NAME ", RequestedId>>),
                    id_manager(Id, Tried_Ids);
                false ->
                    id_manager(Id, Tried_Ids)
            end
    end.


% Proceso que se encarga de enviar un id y esperar un invalid_name o timeout
id_requester(ManagerPid, Port, Socket, BroadcastAddr) ->
    Try_id = generate_ID(),
    ManagerPid ! {try_id, Try_id},
    gen_udp:send(Socket, BroadcastAddr, Port, <<"NAME_REQUEST ", Try_id>>),

    % Se queda escuchando durante 45 segundos si algun otro proceso le dice algo
    receive
        invalid_name ->
            %Mostrar en pantalla que cual Id fallo
            id_requester(ManagerPid, Port, Socket, BroadcastAddr)
    after
        45000 -> ManagerPid ! {set_id, Try_id}
    end.


%----------------------------------------------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------------------


udp_listener(Socket, Broad_PID, Manager_PID) ->
    case gen_udp:recv(Socket, 0) of
        {ok, {Addr, Port, <<"NAME_REQUEST ", IdBin:32/bitstring, "\n">>}} ->
            Id = binary_to_list(IdBin),
            % Manager ! {}
            % receive
            % Confirmarte si es valido
            % Tengo dos opciones:
            %   -Si el ID no es valido, le mando {invalid_name, Pretended_Id}.
            %   -No le mando nada.
            io:format("MSG from ~w:~i NAME_REQUEST ~s ~n", [Addr, Port, Id]),
            % Manager_PID ! {name_request, Id, Socket, Port, Addr},
            udp_listener(Socket, Broad_PID, Manager_PID),
            true;
        {ok, {Addr, Port, <<"HELLO ", IdBin:32/bitstring, " ", PortTCPBinN/binary>>}} ->
            Id = binary_to_list(IdBin),
            [PortTCPBin, <<>>] = binary:split(PortTCPBinN, <<"\n">>),
            PortTCP = binary_to_integer(PortTCPBin),
            %   Almaceno info en know_nodes.json
            io:format("MSG from ~w:~i HELLO ~s ~w ~n", [Addr, Port, Id, PortTCP]),
            udp_listener(Socket, Broad_PID, Manager_PID),
            true;
        {ok, {Addr, Port, <<"INVALID_NAME ", IdBin:32/bitstring, "\n">>}} ->
            Id = binary_to_list(IdBin),
            io:format("MSG from ~w:~i INVALID_NAME ~s ~n", [Addr, Port, Id]),

            %% ¿Corresponde al id que estoy tratando de obtener?
            % Manager_PID ! {self(), get_tried_ids},
            % receive {Manager_PID, Tried_Ids} -> ok end,
            % case Tried_Ids of
            %     [] -> ok;
            %     _  -> 
            %         case Id of
            %             Tried_Id -> Broad_PID ! invalid_name; % Si el proceso ya murió no importa. Erlang descarta el mensaje.
            %             _ -> ok
            %         end
            % end,
            udp_listener(Socket,Broad_PID, Manager_PID),
            true
    end.


%----------------------------------------------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------------------


udp_announcement(Socket, UDP_PORT, TCP_PORT) ->

    receive % No debería quedarse esperando ningún mensaje.
        %  Se puede poner un sleep. 
        {ok, Id} -> 
            erlang:send_after(20000, self(), {ok, Id}),
            Msg = "HELLO " ++ Id ++ " " ++ integer_to_list(TCP_PORT) ++ "\n",
            gen_udp:send(Socket, {255, 255, 255, 255}, UDP_PORT, Msg),
            udp_announcement(Socket, UDP_PORT, TCP_PORT)
    end.
