// Package resolver takes an AST and performs type checking on the it and all its children
// to resolve Identifiers and Type information. All data returned comes from the types package,
// and is attached to the AST with the Identifiers.
package resolver

import (
	"fmt"

	"github.com/masp/garlang/ast"
	"github.com/masp/garlang/printer"
	"github.com/masp/garlang/token"
	"github.com/masp/garlang/types"
)

type context struct {
	scope *Scope
}

type resolver struct {
	ctx     context
	symbols *types.SymbolTable

	file    *token.File
	errlist token.ErrorList
}

func (r *resolver) error(node ast.Node, format string, args ...any) {
	r.errlist.Add(r.file.Position(node.Pos()), fmt.Errorf(format, args...))
}

// Scope is a symbol table for a given Scope, for example a function
// body or an if-statement. Every Scope can only have one declaration per
// name, where duplicate names override previous ones. Scope is only meant to be
// used during resolution.
type Scope struct {
	Outer   *Scope
	Symbols map[string]types.Decl
}

func NewScope(outer *Scope) *Scope {
	return &Scope{Outer: outer, Symbols: make(map[string]types.Decl)}
}

func (s *Scope) Lookup(name string) types.Decl {
	if obj, ok := s.Symbols[name]; ok {
		return obj
	}

	if s.Outer != nil {
		return s.Outer.Lookup(name)
	}
	return types.Decl{}
}

func (s *Scope) Insert(name string, decl types.Decl) (found types.Decl) {
	s.Symbols[name] = decl
	return decl
}

// ResolveModule walks the AST trying to match every identifier to a declaration in scope. If a match is found, it
// is returned in the symbol table. If a match is not found, the entry is returned in Unresolved. All identifiers
// must be either resolved or unresolved by the end of the walk.
func ResolveModule(module *ast.Module, outer *Scope) (*types.SymbolTable, error) {
	r := &resolver{
		symbols: types.NewSymbolTable(),
		file:    module.File,
	}

	r.ctx.scope = outer
	r.pushScope()
	r.resolveModule(module)
	return r.symbols, r.errlist.Err()
}

func ResolveExpr(file *token.File, expr ast.Expression, outer *Scope) (*types.SymbolTable, error) {
	r := &resolver{
		symbols: types.NewSymbolTable(),
		file:    file,
	}

	r.ctx.scope = outer
	r.pushScope()
	r.resolveExpr(expr)
	return r.symbols, r.errlist.Err()
}

// resolve recursively traverses the expression and declares all identifiers the given scope as well as defining
// subscopes for each block found.
func (r *resolver) resolveModule(x *ast.Module) {
	r.declare(x.Id, x, &types.Atom{Value: x.Id.Name})
	// TODO: Declare function names and constants
	for _, decl := range x.Decls {
		if fd, ok := decl.(*ast.FuncDecl); ok {
			r.resolveFunc(fd)
		}
	}
}

func (r *resolver) resolveFunc(x *ast.FuncDecl) {
	r.pushScope() // function scope
	defer r.popScope()

	// TODO: Declare parameters and manually specified types

	// TODO: Resolve body
	for _, stmt := range x.Statements {
		r.resolveStmt(stmt)
	}
}

func (r *resolver) resolveStmt(stmt ast.Statement) {
	switch s := stmt.(type) {
	case *ast.ExprStatement:
		r.resolveExpr(s.Expression)
	default:
		panic(fmt.Sprintf("unhandled statement type %T", stmt))
	}
}

func (r *resolver) resolveExpr(expr ast.Expression) types.Type {
	switch e := expr.(type) {
	case *ast.MatchAssignExpr:
		return r.resolveMatchAssign(e)
	case *ast.Identifier:
		decl := r.lookup(e.Name)
		if decl.Id == nil {
			r.missing(e)
			return types.Invalid
		}
		r.symbols.Resolved[e] = decl
		return decl.Type
	case *ast.IntLiteral:
		return types.UntypedInt
	case *ast.FloatLiteral:
		return types.UntypedFloat
	case *ast.StringLiteral:
		return types.UntypedString
	case *ast.AtomLiteral:
		return &types.Atom{Value: e.Value}
	case *ast.BinaryExpr:
		return r.resolveBinaryExpr(e)
	case *ast.UnaryExpr:
		return r.resolveUnaryExpr(e)
	default:
		panic(fmt.Errorf("unsupported expression %T", expr))
	}
}

func (r *resolver) mustPrint(node ast.Node) string {
	s, err := printer.Print(r.file, node)
	if err != nil {
		return "bad"
	}
	return s
}

func (r *resolver) resolveBinaryExpr(expr *ast.BinaryExpr) types.Type {
	leftT := r.resolveExpr(expr.Left)
	rightT := r.resolveExpr(expr.Right)
	switch expr.Op {
	case token.Plus:
		// Numeric operators OR string concatenation
		if !types.IsString(leftT) && !types.IsNumeric(leftT) {
			r.error(expr.Left, "operator %s not defined on %s (%s)", printer.TokenTable[expr.Op], r.mustPrint(expr.Left), leftT)
			return types.Invalid
		}
		if !types.IsString(rightT) && !types.IsNumeric(rightT) {
			r.error(expr.Right, "operator %s not defined on %s (%s)", printer.TokenTable[expr.Op], r.mustPrint(expr.Right), rightT)
			return types.Invalid
		}

		result, err := types.ApplyOp(leftT, rightT)
		if err != nil {
			r.error(expr, "operator %s has mismatched types: %s and %s", printer.TokenTable[expr.Op], leftT, rightT)
			return types.Invalid
		}
		return result
	case token.Minus, token.Star, token.Slash, token.Less, token.LessEqual, token.Greater, token.GreaterEqual:
		// Numeric operators
		if !types.IsNumeric(leftT) {
			r.error(expr.Left, "operator %s not defined on %s (%s)", printer.TokenTable[expr.Op], r.mustPrint(expr.Left), leftT)
			return types.Invalid
		}
		if !types.IsNumeric(rightT) {
			r.error(expr.Right, "operator %s not defined on %s (%s)", printer.TokenTable[expr.Op], r.mustPrint(expr.Right), rightT)
			return types.Invalid
		}

		resultT, err := types.ApplyOp(leftT, rightT)
		if err != nil {
			r.error(expr, "operator %s has mismatched types: %s and %s", printer.TokenTable[expr.Op], leftT, rightT)
			return types.Invalid
		}
		return resultT
	case token.BangEqual, token.EqualEqual:
		// Boolean operators
		if leftT != rightT {
			r.error(expr, "operator %s has mismatched types: %s and %s", printer.TokenTable[expr.Op], leftT, rightT)
			return types.Invalid
		}
		return types.Bool
	default:
		panic(fmt.Errorf("unhandled binary operator: %s", expr.Op))
	}
}

func (r *resolver) resolveUnaryExpr(expr *ast.UnaryExpr) types.Type {
	switch expr.Op {
	case token.Minus, token.Plus:
		t := r.resolveExpr(expr.Right)
		if !types.IsNumeric(t) {
			desc, _ := printer.Print(r.file, expr.Right)
			r.error(expr.Right, "operator %s not defined on %s (%s)", printer.TokenTable[expr.Op], desc, t)
			return types.Invalid
		}
		return t
	case token.Bang:
		t := r.resolveExpr(expr.Right)
		if !types.Bool.IsSubset(t) {
			desc, _ := printer.Print(r.file, expr.Right)
			r.error(expr.Right, "operator %s not defined on %s (%s)", printer.TokenTable[expr.Op], desc, t)
			return types.Invalid
		}
		return types.Bool
	default:
		panic("unrecognized unary operator: " + expr.Op.String())
	}
}

func (r *resolver) pushScope() *Scope {
	scope := NewScope(r.ctx.scope)
	r.ctx.scope = scope
	return scope
}

func (r *resolver) popScope() {
	r.ctx.scope = r.ctx.scope.Outer
}

// resolveMatchAssign evaluates the expression on the left hand side and unifies it with the
// types of the right hand side. The unification algorithm is recursive:
// - If the left hand side is an identifier, it is always unified with the right hand side
// - If the left hand side is a tuple, each element is resolved with the corresponding element on the right hand side
//
// If all variables are already assigned, an error is returned.
func (r *resolver) resolveMatchAssign(m *ast.MatchAssignExpr) types.Type {
	switch left := m.Left.(type) {
	case *ast.Identifier:
		right := r.resolveExpr(m.Right)
		if types.IsUntyped(right) {
			right = -right.(types.BasicType)
		}
		r.declare(left, m, right)
		return right
	default:
		r.error(m.Left, "unsupported lhs for match assign: %T", left)
	}
	return types.Invalid
}

func (r *resolver) declare(id *ast.Identifier, decl ast.Node, typ types.Type) types.Decl {
	d := types.Decl{Id: id, RefersTo: decl, Type: typ}
	r.symbols.Resolved[id] = d
	return r.ctx.scope.Insert(id.Name, d)
}

func (r *resolver) lookup(name string) types.Decl {
	return r.ctx.scope.Lookup(name)
}

// missing adds a new identifier to be associated in the symbol table as missing.
func (r *resolver) missing(ident *ast.Identifier) {
	r.symbols.Unresolved[ident] = struct{}{}
}
