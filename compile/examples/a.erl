-module(a).
-export([a/0]).

% compile:file("sample.core", [from_core]).

a() -> 1.

multi_call() ->
    (a()):b(1).