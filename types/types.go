package types

import (
	"fmt"
	"strings"

	"github.com/masp/ertylang/ast"
)

type Node interface {
	// Node represents any ast.Node that can have its type changed
	SetType(t ast.Type)
}

// Untyped is a placeholder type for when the type depends on the context in which it is used
// For example:
// - const a = 100
// - func(a) // if func accepts an int, then a is an int. If func accepts a float, then a is a float.
// It also is the type of unresolved identifiers.
type BasicType int

const (
	Invalid    BasicType = 0
	ModuleName BasicType = 124 // module names are technically atoms, but the typechecker can enforce that an atom imported is used.
	Atom       BasicType = 125 // represents the infinite class of atoms, not a specific atom. See AtomValue.
	Void       BasicType = 126
	Any        BasicType = 127

	// these are set for constants. They are coercible to the respective types as constants. See Coerce()
	UntypedInt    BasicType = -1
	UntypedFloat  BasicType = -2
	UntypedString BasicType = -3

	Int    BasicType = 1
	Float  BasicType = 2
	String BasicType = 3
)

func (b BasicType) Underlying() ast.Type { return b }
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
	case Any:
		return "any"
	case Void:
		return "void"
	case Atom:
		return "atom"
	case ModuleName:
		return "module name"
	default:
		panic("unreachable")
	}
}
func IsUntyped(t ast.Type) bool {
	if t, ok := t.(BasicType); ok && t < 0 {
		return true
	}
	return false
}
func MakeTyped(t ast.Type) ast.Type {
	if IsUntyped(t) {
		return -t.(BasicType)
	}
	return t
}

type AtomValue struct {
	V string // Values of tuples can also be unique types
}

func (t *AtomValue) Underlying() ast.Type { return t }
func (t *AtomValue) String() string       { return "atom '" + t.V + "'" }

func IsNumeric(t ast.Type) bool {
	return t.Underlying() == Int ||
		t.Underlying() == Float ||
		t.Underlying() == UntypedInt ||
		t.Underlying() == UntypedFloat ||
		t == Any
}

func IsString(t ast.Type) bool {
	return t.Underlying() == String || t.Underlying() == UntypedString
}

func IsAtom(t ast.Type) bool {
	if t.Underlying() == Atom {
		return true
	}
	if t, ok := t.(*AtomValue); ok && t.V != "" {
		return true
	}
	return false
}

type Module struct {
	AtomValue

	Imported *ast.Module // the resolved and imported module (nil if unsuccessful)
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
	Cases []ast.Type
}

func (e *Enum) Underlying() ast.Type { return e }
func (e *Enum) String() string {
	var parts []string
	for _, c := range e.Cases {
		parts = append(parts, c.String())
	}
	return "enum (" + strings.Join(parts, ", ") + ")"
}

var Bool = &Enum{
	Cases: []ast.Type{
		&AtomValue{V: "true"},
		&AtomValue{V: "false"},
	},
}

func (e *Enum) Intersect(t ast.Type) ast.Type {
	for _, c := range e.Cases {
		if IsAssignable(c, t) {
			return c // return case that matches
		}
	}

	if other, ok := t.(*Enum); ok {
		for _, c := range other.Cases {
			if e.Intersect(c) == Invalid {
				return Invalid // all cases must be a subset of other enum
			}
		}
		return other
	}
	return Invalid
}

type Func struct {
	Args   []ast.Type
	Return ast.Type
}

func (f *Func) Underlying() ast.Type { return f }
func (f *Func) String() string {
	var args []string
	for _, t := range f.Args {
		args = append(args, t.String())
	}
	return fmt.Sprintf("func(%s) %s", strings.Join(args, ", "), f.Return)
}

type Overload []*Func

func (o Overload) Underlying() ast.Type { return o }
func (o Overload) String() string {
	var parts []string
	for _, f := range o {
		parts = append(parts, f.String())
	}
	return strings.Join(parts, ", ")
}

// Expr represents a type declaration, for example `type A int` int is a type expression as well as
// A. For int, Definition = types.Int, for A, Definition -> int -> types.Int.
type Expr struct {
	Definition ast.Type
}

func (t *Expr) Underlying() ast.Type {
	var ok bool
	u := t.Definition
	for {
		if _, ok = u.(*Expr); ok {
			u = u.(*Expr).Definition
		} else {
			return u
		}
	}
}
func (t *Expr) String() string { return t.Definition.String() }

type List struct {
	Elem ast.Type // the type of the elements or types.Invalid if empty list
}

func (l *List) IsEmpty() bool        { return l.Elem == Invalid }
func (l *List) Underlying() ast.Type { return l }
func (l *List) String() string {
	if l.Elem == Invalid {
		return "[]"
	}
	return fmt.Sprintf("[]%s", l.Elem)
}
