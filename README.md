# About Garlang

Work in progress.

A hobby project to see test out building a BEAM language. The goal of Garlang is to be closely related to Erlang, but with a few differences:
- Typing is enforced, but not strictly (similar to TypeScript)
- Syntax is more modernized (similar to Go)
- Functional strictness of Erlang is de-emphasized with more emphasis on immutability and pattern matching (e.g. "assignment" is allowed)
- Easy 1-1 conversion and interop between Erlang and this language (erl2gar conversion utility)

## Example
    
```erlang
-module(hello).
-export([hello/0]).

hello() ->
    io:format("Hello, world!~n").
```

```garlang
module hello

export func hello() {
    io.format("Hello, world!~n")
}
```

### Usage
```
gar build -o examples/ examples/hello_world.gar
```


