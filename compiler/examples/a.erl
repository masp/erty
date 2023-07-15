-module(a).
-export([add/1, main/0, nest/5]).

% compile:file("sample.core", [from_core]).

add(A) -> A + 90.
main() ->
    io:format(<<"~p~n">>, [add(1)]).

nest(A, B, C, D, E) ->
    (A + B) + (erlang:call(C, D)) - E.