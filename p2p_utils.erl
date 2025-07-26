-module(p2p_utils).
-export([generate_ID/0, debug/2]).

-define(DEBUG, true).
-if(?DEBUG).
    debug(Format, Data) -> io:format(Format, Data).
-else.
    debug(_Format, _Data) -> ok.
-endif.

generate_ID() ->
    Size = 4, % Tamaño en bytes del ID
    Alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789", % Caracteres válidos
    NAlpha = length(Alpha),
    lists:map(fun(_) -> lists:nth(rand:uniform(NAlpha), Alpha) end, lists:seq(1,Size)).