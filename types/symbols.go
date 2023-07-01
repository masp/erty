// Package types describes the hierarchy of types for the language. The types
// can be built-in or user-defined. Types are attached to AST nodes as part of
// type resolution. Applying type resolution to an AST producces a 1-1 mapping
// from every identifier to a type, as well as a reference to the declaration from
// which the type came.
package types

import "github.com/masp/garlang/ast"

type Decl struct {
	Id *ast.Identifier
	// If Node is *ast.Identifier, then RefersTo points to the declaration that this identifier refers to
	// Examples:
	// - Function -> *ast.FuncDecl
	// - Module -> *ast.ImportDecl
	// - Type -> *ast.TypeDecl
	// - Variable -> *ast.AssignMatchExpr
	RefersTo ast.Node
	Type     Type // Never nil
}

func NewDecl(id *ast.Identifier, typ Type) Decl {
	return Decl{Id: id, RefersTo: id, Type: typ} // decls refer to themselves
}

type SymbolTable struct {
	Resolved   map[*ast.Identifier]Decl
	Unresolved map[*ast.Identifier]struct{} // present if it is unresolved
}

func NewSymbolTable() *SymbolTable {
	return &SymbolTable{
		Resolved:   make(map[*ast.Identifier]Decl),
		Unresolved: make(map[*ast.Identifier]struct{}),
	}
}
