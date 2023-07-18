# Match expression
In Erlang, the ability to pattern match is a crucial part of the language. It allows you to match on the structure of data and bind variables to parts of that structure.

Goals:
- Minimize variants of syntax for pattern matching
- Concise syntax without sacrificing clarity

## Goal 1: Minimize variants of syntax for pattern matching
Currently, there are 3 different ways to pattern match in Erlang:
- `=` operator
- Implicit match within an function call argument
- Case expression and similar expressions

All three can bind and enforce. Bind meaning that variables can be bound to parts of the structure. Enforce meaning that the pattern must match the structure of the data to evaluate.

### `=` operator

The `=` operator is the most common way to pattern match in Erlang. It is used to bind and enforce. It is also used to rebind variables.

```erlang
1> X = 1.
1
2> 1 = X.
1
3> 2 = X.
** exception error: no match of right hand side value 1
```

The `no match` exception is crucial to error handling semantics in Erlang:

```erlang
1> {ok, X} = call_api().
{ok, ...}
```

To separate the binding and enforcing semantics would require something like:

```erlang
1> {R, X} = call_api().
2> case R of
3>  ok -> ok;
4>  error -> erlang:throw({error, X})
5> end.
```

This is much more verbose, but clearly communicates the possibility of an error. Erlang, unlike Go or Rust, has a robust runtime and platform to handle unexpected errors, so unexpected errors is par for the course. For this reason, ertylang supports binding and enforcing with the `=` operator just like Erlang.

```ertylang
{'ok', X} = call_api()
```

Non-explicit error handling does cause challenges for applications like CLIs where you want to communicate errors more cleanly than stack traces from crashes. Since ertylang favors consistency with Erlang, it will favor implicit error handling over explicit.

#### Types with `=`
Imagine a type like:

```erlang

start_opt() =
    {timeout, Timeout :: timeout()} |
    {spawn_opt, SpawnOptions :: [proc_lib:spawn_option()]} |
```


```ertylang
type Timeout interface {
    int
    'infinity'
}

type SpawnOptions interface {
    'link'
    tuple['priority', erlang.priority_level]
    tuple['fullsweep_after', int]
    tuple['min_heap_size', int]
    tuple['min_bin_vheap_size', int]
    tuple['max_heap_size', erlang.max_heap_size]
    tuple['message_queue_data', erlang.message_queue_data]
}

type StartOpt interface {
    tuple{'timeout', Timeout}
    tuple{'spawn_opt', []SpawnOpt}
}

type Result interface {
    tuple{'ok', tup{}}
    tuple{'ok', any}
}
```

A binding looks like:

```
// Tuples
let A = 3 // A is assigned type int
let {A} = tuple{int}{3} // A is assigned type int
{'ok', A int} = {'ok', 3} // A is type int, A must be declared before

// Lists
let A string | []string = "a" | []string{"b", "c"} // A is assigned
let {'ok', A string} = {'ok', "a"} // A is assigned type string
let map{'key': A string} = map{'key': "a"}
```

## Match
A match expression is a special syntax which allows you to match on certain types and values
of variables and interfaces. 

```
type opts map{
    'timeout': int,
    'spawn_opt': []SpawnOpt
}

type result interface {
    tuple{'ok', []string}
    tuple{'ok', []int}
}

let result = {'ok', []string{"a", "b", "c"}}
let A = match result {
case []int{A, ...}:
    {'ok', A}
case []:
    {'ok', 0}
case A string:
    {'ok', A}
}
```

### Ambiguity of type match on interface

```ertylang
type array interface {
    []int
    []float
}

func match(in array) int {
    match in.(type) {
    case []int: // in is []int
        sumInts(in)
    case []float:
        sumFloats(in)
    }
}


type result interface {
    tuple['ok', []string] // allowed, but impossible to distinguish on
    tuple['ok', []int]
}

func match(in result) int {
    let a tup{int, int} = {1, 2}
    let b []string = [1, 2]
    a := {1, 2}
    a = {1, 2}
    match in {
    case {'ok', []int{H, T...}}: // A is type int (how do we know? when is_integer(A) is true)
        sumInts(in)
    case {'ok', []string[H, T...]}:
        sumFloats(in)
    }
}
```
```

