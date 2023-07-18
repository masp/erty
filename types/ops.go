package types

import (
	"errors"
	"fmt"

	"github.com/masp/ertylang/ast"
)

var (
	ErrLHS      = errors.New("op not defined on LHS expression")
	ErrRHS      = errors.New("op not defined on RHS expression")
	ErrMismatch = errors.New("op not defined on mismatched types")
)

var arithmTable = map[BasicType]map[ast.Type]ast.Type{
	UntypedInt: {
		Int:          Int,
		Float:        Float,
		UntypedInt:   UntypedInt,
		UntypedFloat: UntypedFloat,
	},
	UntypedFloat: {
		Int:          Float,
		Float:        Float,
		UntypedInt:   UntypedFloat,
		UntypedFloat: UntypedFloat,
	},
	UntypedString: {
		String:        String,
		UntypedString: UntypedString,
	},
}

func ApplyOp(t1, t2 ast.Type) (ast.Type, error) {
	if t1 == t2 {
		return t1, nil // No types are mismatched
	}

	if !IsUntyped(t1) && !IsUntyped(t2) {
		return nil, ErrMismatch
	}

	var (
		untyped BasicType
		other   ast.Type
	)
	if IsUntyped(t1) {
		untyped = t1.(BasicType)
		other = t2
	} else {
		untyped = t2.(BasicType)
		other = t1
	}

	arithm := arithmTable[untyped]
	if match, ok := arithm[other]; ok {
		return match, nil
	}
	return nil, ErrMismatch
}

// Assignable(to, value) accepts a variable and value type and returns an error or nil according to following rules:
// 1. Assignable(int, untyped int) -> nil
// 2. Assignable(int, untyped float) -> cannot use untyped float as int
// 3. Assignable(int, untyped string) -> cannot use untyped string as int
// 4. Assignable(float, untyped int) -> nil
// 5. Assignable(float, untyped float) -> nil
// 6. Assignable(float, untyped string) -> cannot use untyped string as float
// 7. Assignable(string, untyped string) -> nil
//
// The rules above apply the same if the var type is user-define (var.Underlying() != var).
func IsAssignable(to, value ast.Type) ast.Type {
	value = Deref(value)
	to = Deref(to)
	if toExpr, ok := to.(*Expr); ok {
		// If we're trying to assign to a type expression, use the type definition instead
		to = toExpr.Underlying()
	}

	if to == Any {
		return to
	}

	if IsUntyped(value) && isConvertible(to.Underlying(), value) {
		return to
	}
	if toAtom, ok := to.(*AtomValue); ok {
		if valueAtom, ok := value.(*AtomValue); ok {
			if toAtom.V == valueAtom.V {
				return to
			}
		}
	}
	if to == value {
		return to
	}
	return Invalid
}

func isConvertible(to, value ast.Type) bool {
	switch to {
	case Int:
		switch value {
		case UntypedInt:
			return true
		default:
			return false
		}
	case Float:
		switch value {
		case UntypedInt, UntypedFloat:
			return true
		default:
			return false
		}
	case String:
		switch value {
		case UntypedString:
			return true
		default:
			return false
		}
	}
	return false
}

func Cast(from, to ast.Type) (ast.Type, error) {
	if from == Any {
		return to, nil
	}

	if to == from {
		return to, nil
	}

	if to.Underlying() == from.Underlying() {
		return to, nil // Actual values are equivalent, just different aliases
	}

	if IsAssignable(to, from) != Invalid {
		return to, nil
	}
	return Invalid, fmt.Errorf("non-castable types")
}
