-module(a).
-export([add/1, main/1]).

% compile:file("sample.core", [from_core]).

add(A) -> A + 90.
main(A) ->
    case A of
        {ok, 1} -> true;
        {bad, 2} -> false
    end.