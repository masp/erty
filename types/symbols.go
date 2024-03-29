// Package types describes the hierarchy of types for the language. The types
// can be built-in or user-defined. Types are attached to AST nodes as part of
// type resolution. Applying type resolution to an AST producces a 1-1 mapping
// from every identifier to a type, as well as a reference to the declaration from
// which the type came.
package types

import "github.com/masp/ertylang/ast"

// Decl represents a type that comes from a reference to a declaration. It includes info
// on what the type is, as well as a reference to the declaration that it came from.
type Decl struct {
	// If Node is *ast.Identifier, then RefersTo points to the declaration that this identifier refers to
	// Examples:
	// - Function -> *ast.FuncDecl
	// - Module -> *ast.ImportDecl
	// - Type -> *ast.TypeDecl
	// - Variable -> *ast.AssignMatchExpr
	RefersTo ast.Node
	ast.Type // Never nil
}

// Value returns the type if it were applied to a value. If t is an expression or decl, it returns the
// type that it represents to or declares.
func Value(t ast.Type) ast.Type {
	if t, ok := t.(*Expr); ok {
		return Value(t.Definition)
	}
	if t, ok := t.(*Decl); ok {
		return Value(t.Type)
	}
	return t
}

func NewDecl(id *ast.Identifier, typ ast.Type) *Decl {
	return &Decl{RefersTo: id, Type: typ} // decls refer to themselves
}

// DeclNode checks if type is a decl and if it is, returns the ast.Node that it refers to.
// If it is not a *Decl, it returns nil.
func DeclNode(t ast.Type) ast.Node {
	if t, ok := t.(*Decl); ok {
		return t.RefersTo
	}
	return nil
}

// Deref returns the type that t refers to. If t is not a *Decl, it returns t.
func Deref(t ast.Type) ast.Type {
	if t, ok := t.(*Decl); ok {
		return t.Type
	}
	return t
}
