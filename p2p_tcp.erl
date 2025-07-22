-module(p2p_tcp).
-export([tcp_listener/2]).


tcp_listener(Ip, TCP_PORT) ->
    case gen_tcp:listen(TCP_PORT).