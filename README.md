# About

> Work in progress.

Erty is Erlang but with stronger support for types (ERlang with TYpes).

Erty's primary differences from Erlang are:
- Strongly enforced, static typing
- C family, imperative syntax (based on Golang)
- Easy interop to and from Erlang

Unlike Elixir, Erty makes very few extensions to the core features of Erlang. Erty is intended to be easy to drop into a large, existing Erlang project and be beneficial immediately with a low learning curve.

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

To compile files:
```
ert build -o examples/ examples/hello_world.ert
```




