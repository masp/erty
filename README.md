# About

> Work in progress.

Erty is Erlang built from scratch to be more friendly to programmers coming from C/Java/Python/Typescript.

Erty is closely related to Erlang, but with the following differences:
- Enforced, static typing
- Modern syntax (inspired by Go)
- Easy interop between Erlang

## Example

```erty
module hello

func hello() {
    io.format("Hello, world!~n")
}
```

compiles to

```erlang
-module(hello).
-export([hello/0]).

hello() ->
    io:format("Hello, world!~n").
```

### Usage

To execute a `ert` file with a `main` function:
```
ert run examples/hello_world.ert
```

To load a module interactively into an Erlang shell:
```
ert run -shell examples/hello_world.ert
```

To build core files:
```
ert build -o examples/ examples/hello_world.ert
```




