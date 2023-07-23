package resolver

import (
	"github.com/masp/ertylang/ast"
	"github.com/masp/ertylang/types"
)

// Scope is a symbol table for a given Scope, for example a function
// body or an if-statement. Every Scope can only have one declaration per
// name, where duplicate names override previous ones. Scope is only meant to be
// used during resolution.
type Scope struct {
	Outer   *Scope
	Symbols map[string]*types.Decl
	currFn  *ast.FuncDecl // ptr to current function body (nil if scope is not defined by func)
}

func NewScope(outer *Scope) *Scope {
	return &Scope{Outer: outer, Symbols: make(map[string]*types.Decl)}
}

func (s *Scope) Lookup(name string) *types.Decl {
	if obj, ok := s.Symbols[name]; ok {
		return obj
	}

	if s.Outer != nil {
		return s.Outer.Lookup(name)
	}
	return nil
}

func (s *Scope) CurrentFunc() *ast.FuncDecl {
	if s.currFn != nil {
		return s.currFn
	}
	if s.Outer != nil {
		return s.Outer.CurrentFunc()
	}
	return nil
}

func (s *Scope) Insert(name string, decl *types.Decl) (found *types.Decl) {
	s.Symbols[name] = decl
	return decl
}

var Universe *Scope
var builtInDecl *ast.BadDecl // a token value indicating the value is a built-in

func init() {
	Universe = NewScope(nil)
	for id, typ := range types.Builtins {
		Universe.Insert(id, &types.Decl{
			Type:     typ,
			RefersTo: builtInDecl,
		})
	}
}
