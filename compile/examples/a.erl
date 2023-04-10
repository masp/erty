-module(a).
-export([a/0]).

% compile:file("sample.core", [from_core]).

-file("sample.core", 3).
a() -> a.