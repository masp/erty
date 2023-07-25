-module(a).
-export([add/1, main/0, cases/1, fib/1]).

% compile:file("sample.core", [from_core]).

add(A) -> A + 90.
main() ->
    io:format(<<"~p~n">>, [add(1)]).

% fib(0) -> 0;
% fib(1) -> 1;
% fib(N) -> fib(N-1) + fib(N-2).

fib(N) ->
    case N of
        0 -> 0;
        1 -> 1;
        A -> fib(A-1) + fib(A-2)
    end.

cases(A) ->
    C = 10,
    case add(A)+10 of
        10 -> 10;
        C -> C;
        {10, Z} -> Z;
        {10, 20} -> 30;
        B -> B
    end.