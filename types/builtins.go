package types

// A Type represents any type in the language.
type Type interface {
	// Underlying returns the underlying type of a type. An underlying type must be one of the built in types.
	Underlying() Type

	// String returns a string representation of a type.
	String() string
}

// Untyped is a placeholder type for when the type depends on the context in which it is used
// For example:
// - const a = 100
// - func(a) // if func accepts an int, then a is an int. If func accepts a float, then a is a float.
// It also is the type of unresolved identifiers.
type BasicType int

const (
	Invalid BasicType = 0

	// these are set for constants. They are coercible to the respective types as constants. See Coerce()
	UntypedInt    BasicType = -1
	UntypedFloat  BasicType = -2
	UntypedString BasicType = -3

	Int    BasicType = 1
	Float  BasicType = 2
	String BasicType = 3
)

func (b BasicType) Underlying() Type { return b }
func (b BasicType) String() string {
	switch b {
	case Invalid:
		return "invalid"
	case Int:
		return "int"
	case Float:
		return "float"
	case String:
		return "string"
	case UntypedInt:
		return "untyped int"
	case UntypedFloat:
		return "untyped float"
	case UntypedString:
		return "untyped string"
	default:
		panic("unreachable")
	}
}
func IsUntyped(t Type) bool {
	if t, ok := t.(BasicType); ok && t < 0 {
		return true
	}
	return false
}

type Atom struct {
	Value string // Values of tuples can also be unique types
}

func (t *Atom) Underlying() Type { return t }
func (t *Atom) String() string   { return "atom '" + t.Value + "'" }

func IsNumeric(t Type) bool {
	return t.Underlying() == Int || t.Underlying() == Float || t.Underlying() == UntypedInt || t.Underlying() == UntypedFloat
}

func IsString(t Type) bool {
	return t.Underlying() == String || t.Underlying() == UntypedString
}

// Enum defines what types of values can be assigned to a variable. It is mostly useful for message passing
// in Erlang where types of values can be specified and auto-generated switch statements are made very easy. Behavior interfaces
// are more useful when you want to define abstract behavior.
//
//	type msg enum {
//		'ok'
//	 'error'
//	}
type Enum struct {
	Cases []Type
}

func (e *Enum) Underlying() Type { return e }
func (e *Enum) String() string   { return "enum" }

var Bool = &Enum{
	Cases: []Type{
		&Atom{Value: "true"},
		&Atom{Value: "false"},
	},
}

func (e *Enum) IsSubset(t Type) bool {
	for _, c := range e.Cases {
		if IsAssignable(c, t) != Invalid {
			return true
		}
	}

	if other, ok := t.(*Enum); ok {
		all := true
		for _, c := range other.Cases {
			if !e.IsSubset(c) {
				return false // all cases must be a subset of other enum
			}
		}
		return all
	}
	return false
}
