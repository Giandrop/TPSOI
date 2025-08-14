-module(p2p_udp).
-import(p2p_utils, [generate_ID/0, debug/2]).
-export([id_manager/2, id_requester/2, udp_listener/2, udp_announcement/3, ips/0]).

-define(TIMEOUT, 15000).
-define(BROADADDR, {255,255,255,255}).

id_manager(Id, Tried_Ids) ->
    receive
        {Pid, get_id} -> 
            Pid ! {self(), Id},
            id_manager(Id, Tried_Ids);
        {Pid, get_tried_ids} ->
            Pid ! {self(), Tried_Ids},
            id_manager(Id, Tried_Ids);
        {set_id, NewId} -> 
            debug("Setting Id: ~s~n", [NewId]),
            id_manager(NewId, Tried_Ids);
        {try_id, Try_id} -> 
            id_manager(Id, [Try_id | Tried_Ids]);
        {name_request, RequestedId, Socket, Port, Addr} ->
            case RequestedId =:= Id orelse lists:member(RequestedId, Tried_Ids) of
                true ->
                    io:format("~s in Tried_ids ~n", [RequestedId]),
                    IdBin = list_to_binary(RequestedId),
                    % Según Google:
                    % list_to_binary/1 in Elixir is generally not recommended for converting lists
                    % to strings, especially when dealing with Unicode characters, because it 
                    % performs a raw, byte-level conversion and does not inherently understand 
                    % character encodings or Unicode codepoints. This can lead to incorrect results
                    % when attempting to convert lists that represent valid strings with multi-byte
                    % characters. 
                    % Como acá no estamos trabajando con caracteres unicodes, sino que queremos
                    % reconvertir a binario la lista que obtuvimos originalmente de datos UDP,
                    % podemos usar esta función sin problemas
                    gen_udp:send(Socket, Addr, Port, <<"INVALID_NAME ", IdBin/binary, "\n">>),
                    id_manager(Id, Tried_Ids);
                false ->
                    io:format("~s not in Tried_ids ~n", [RequestedId]),
                    id_manager(Id, Tried_Ids)
            end
    end.


% Proceso que se encarga de enviar un id y esperar un invalid_name o timeout
id_requester(Port, Socket) ->
    Try_id = generate_ID(),
    Pid_manager = whereis(id_manager),
    Pid_manager ! {try_id, Try_id},
    IdBin = list_to_binary(Try_id),
    io:format("Sending NAME_REQUEST ~s~n", [Try_id]),
    gen_udp:send(Socket, ?BROADADDR, Port, <<"NAME_REQUEST ", IdBin/binary, "\n">>),

    % Se queda escuchando durante 45 segundos si algun otro proceso le dice algo
    receive
        invalid_name ->
            %Mostrar en pantalla que cual Id fallo
            id_requester(Port, Socket)
    after
        ?TIMEOUT -> Pid_manager ! {set_id, Try_id}
    end.


%----------------------------------------------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------------------

ips() ->
    {ok, Addrs} = inet:getifaddrs(),
    [
         Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
         size(Addr) == 4, Addr =/= {127,0,0,1}
    ].

udp_listener(Socket, Broad_PID) ->
    case gen_udp:recv(Socket, 0) of
        {ok, {Addr, Port, <<"NAME_REQUEST ", IdBin:32/bitstring, "\n">>}} ->
            Addrs = ips(),
            io:format("My addresses ~w~n", [Addrs]),
            case lists:member(Addr, Addrs) of
                true -> ok; % Si los mensajes son de si mismo los ignora
                false -> 
                    Id = binary_to_list(IdBin),
                    io:format("MSG from ~w:~w NAME_REQUEST ~s ~n", [Addr, Port, Id]),
                    whereis(id_manager) ! {name_request, Id, Socket, Port, Addr}
            end,
            udp_listener(Socket, Broad_PID);
        {ok, {Addr, Port, <<"HELLO ", IdBin:32/bitstring, " ", PortTCPBinN/binary>>}} ->
            Addrs = ips(),
            case lists:member(Addr, Addrs) of
                true -> ok;
                false ->
                    Id = binary_to_list(IdBin),
                    [PortTCPBin, <<>>] = binary:split(PortTCPBinN, <<"\n">>),
                    PortTCP = binary_to_integer(PortTCPBin),
                    registry_id ! {update, Id, Addr, PortTCP},
                    io:format("MSG from ~w:~w HELLO ~s ~w ~n", [Addr, Port, Id, PortTCP])
            end,
            udp_listener(Socket, Broad_PID);
        {ok, {Addr, Port, <<"INVALID_NAME ", IdBin:32/bitstring, "\n">>}} ->
            Id = binary_to_list(IdBin),
            io:format("MSG from ~w:~w INVALID_NAME ~s ~n", [Addr, Port, Id]),

            %% ¿Corresponde al id que estoy tratando de obtener?
            Pid_manager = whereis(id_manager),
            Pid_manager ! {self(), get_tried_ids},
            receive {Pid_manager, Tried_Ids} -> ok end,
            case Tried_Ids of
                [Id | _] -> Broad_PID ! invalid_name; % Si el proceso ya murió no importa. Erlang descarta el mensaje.
                _ -> ok
            end,
            udp_listener(Socket,Broad_PID);
        {ok, {Addr, Port, _Msg}} ->
            io:format("Invalid MSG from ~w:~w ~w ~n", [Addr, Port, _Msg]),
            udp_listener(Socket,Broad_PID),
            true
    end.


%----------------------------------------------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------------------


udp_announcement(Socket, UDP_PORT, TCP_PORT) ->

    Pid_manager = whereis(id_manager),
    Pid_manager ! {self(), get_id},
    receive {Pid_manager, Id} -> ok end,
    case Id of
        no_id -> ok;
        _ -> 
            IdBin = list_to_binary(Id),
            TCPBin = list_to_binary(integer_to_list(TCP_PORT)),
            gen_udp:send(Socket, ?BROADADDR, UDP_PORT, <<"HELLO ", IdBin/binary, " ", TCPBin/binary, "\n">>),
            debug("Enviando HELLO ~s ~s~n", [Id, TCPBin])
    end,
    timer:sleep(20000),
    udp_announcement(Socket, UDP_PORT, TCP_PORT).
