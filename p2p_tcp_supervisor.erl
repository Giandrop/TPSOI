-module(p2p_tcp_supervisor).
-behaviour(supervisor).
 
-export([tcp_supervisor/1]).
% tcp_supervisor es la función que corre el proceso

-export([init/1]).
% como le pusimos behaviour supervisor, tenemos que exportar una función init
% que configure como se va a comportar el supervisor

tcp_supervisor(TCP_PORT) ->
    % start_link crea un supervisor como parte del arbol
    supervisor:start_link({local, ?MODULE}, ?MODULE, [TCP_PORT]).
 
init([TCP_PORT]) ->
    % {ok, Port} = application:get_env(port),
    %% Set the socket into {active_once} mode.
    %% See sockserv_serv comments for more details

    % el supervisor crea el socket de escucha
    {ok, ListenSocket} = gen_tcp:listen(TCP_PORT, [binary, {active, false}, {packet, line}]),
    
    % spawnea 20 hijos
    spawn_link(fun empty_listeners/0),
    
    % los hijos tienen esta configuración:
    {ok, {{simple_one_for_one, 60, 3600},
    [{socket,
        {p2p_tcp_node, tcp_listener, [ListenSocket]}, % pass the socket!
        temporary, %% temporary?? 
        1000, worker, [p2p_tcp_node]}
]}}.
 
%% Start with 20 listeners so that many multiple connections can
%% be started at once, without serialization. In best circumstances,
%% a process would keep the count active at all times to insure nothing
%% bad happens over time when processes get killed too much.
empty_listeners() ->
    [supervisor:start_child(?MODULE, []) || _ <- lists:seq(1,20)],
ok.