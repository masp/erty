# Types

Types in Erlang are implemented with specs. They are optional hints that are used by other tools to provide static analysis and documentation. They are not enforced by the runtime. Since they are optional, tools also have limited capabilities with
what they can do. For example, Dialyzer can only check the types of functions that have specs.

Part of what makes types in other languages valuable is instant feedback from the compiler whether code is calling functions, matching
patterns, or passing arguments correctly. This is not possible with Erlang specs.

Garlang will support types that are enforced by the compiler. Types are mandatory for all Garlang code, but is optional for Erlang code. The compiler behaves similar to Typescript, where it tries to be as helpful as possible, but assumes `any` if it's working with typeless Erlang code.

## Goals
1. Types are enforced by the compiler.
2. Types are mandatory everywhere.
3. Easy integration with existing types in Erlang code in tooling and compiler.


# Examples

## Simple types
```garlang
int (64 bit signed integer)
float (64 bit IEEE float)
string (UTf-8 byte sequence, binary in Erlang)
atom (unique named value, atom in Erlang)
```

## Collection types
List - `[]element_type`
Tuple - `tup[e0_type, e1_type, ...]`
Map - `map[key_type]value_type`
Erlang string - `[]int` (list of integers)

## Defining custom types
It's easy to define a type with a different name:

```
type MyInt int
```
which creates a totally unique name that must be exactly the same in order to match. For example,

```
func f(v MyInt) 'ok' { ... }

f(1) // error: expected MyInt, got int
```

If you want to define a type that is a composition of other types, you can either use a struct:

```
type car struct {
    owner string
    price float
}
```

which is just a tuple with named fields. Or you can use an interface:

```
type result interface {
    'error'
    tup{'ok', int}
}
```

which is a enumeration of types that satisfy an interface. In this case, the interface is satisfied by either an atom `error` or a tuple of `ok` and any integer.

## Using types
The basic literals like numbers and strings behave like Erlang:

```
12      // int or float
12.0    // float
"hello" // string
'hello' // atom (unique value (like an enum)! not a string)
```

To instantiate a composite literal of a given type, you use `{` and `}`:

```
[]string{"a", "b", "c"}
map[string]int{"a": 1, "b": 2}
tuple[int, int, int]{1, 2, 3}
// {1, 2, 3} is short for tuple[int, int, int]{1, 2, 3}
```

### Maps
Maps sometimes are used as specs where you want to configure a process with a list of values. Instead of using a map, a struct would provide better syntax checking, but the downside is that optional fields are impossible.

To define a spec map with proplists equivalent could look like:

```
type Option interface {
    tuple['timeout', int]
    tuple['name', string]
}

type Spec []Option // proplist in Erlang
type Spec []tuple['timeout', map[string]string]

func start(spec Spec) 'ok' {
    let A int = proplist.get_value(spec, 'timeout', 5000)
}

```

To define it using a map would look like:

```
type Spec map{
    'timeout': int
    'name': string
}

func start(spec map{'timeout': int, 'name': string}) 'ok' {
    let timeout = match spec {
    case {'timeout': v int}: v
    default: 5000
    }

    let name = match spec {
    case {'name': v}: v
    default: "default"
    }

}
```
```