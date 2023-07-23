package types

import "github.com/masp/ertylang/ast"

// Unify tries to match a pattern expression to a value expression. If the types of the two
// differ, Invalid is returned. If they can be unified, the resulting type is returned. For example,
//
//	types.Unify(int, untyped int) -> int
func Unify(pattern, value ast.Type) ast.Type {
	// If the value can be assigned to a variable with the type, then it can unify.
	result := IsAssignable(pattern, value)
	if result != Invalid {
		return result
	}

	// If one of the types is a subset of the other, than it can be unified. For example,
	// if value is int | string and the pattern is int, these can be unified as int but not string.
	if e, ok := value.(*Enum); ok {
		inter := e.Intersect(pattern)
		if inter != Invalid {
			return inter
		}
	}

	// Otherwise, can't be unified
	return Invalid
}

// Merge takes a set of different types and tries to merge them into the most general type
// possible. The primary function is to collapse untyped types together and build the enum of
// cases.
//
// A naive algorithm is to go through each type and verify if it exists in the set or if it's equivalent
// to one of the existing values in the set. If there are many types to be merged, it might be faster
// to use a map.
func Merge(ts ...ast.Type) ast.Type {
	// Flatten all enums
	flattened := make([]ast.Type, 0, len(ts))
	for _, t := range ts {
		if e, ok := t.(*Enum); ok {
			flattened = append(flattened, e.Cases...)
		} else {
			flattened = append(flattened, t)
		}
	}

	// Deduplicate types
	dedupd := make([]bool, len(flattened))
	var result []ast.Type
	for i, t := range flattened {
		if dedupd[i] {
			continue
		}
		// Find all duplicates of t
		result = append(result, t)
		dedupd[i] = true
		for j := i + 1; j < len(flattened); j++ {
			if IsEqual(t, flattened[j]) {
				dedupd[j] = true
			} else if IsUntyped(flattened[j]) && isConvertible(t, flattened[j]) {
				// Implicitly cast untyped constants to their closest "relatives"
				dedupd[j] = true
			}
		}
	}
	if len(result) == 1 {
		return result[0]
	}
	return &Enum{Cases: result}
}
