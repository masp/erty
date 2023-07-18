// Package resolver takes an AST and performs type checking on the it and all its children
// to resolve Identifiers and Type information. All data returned comes from the types package,
// and is attached to the AST with the Identifiers.
package resolver

import (
	"fmt"

	"github.com/masp/ertylang/ast"
	"github.com/masp/ertylang/printer"
	"github.com/masp/ertylang/token"
	"github.com/masp/ertylang/types"
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
	resolvedArgs := make([]any, len(args))
	for i, arg := range args {
		switch arg := arg.(type) {
		case ast.Node:
			resolvedArgs[i], _ = printer.Print(r.file, arg)
		default:
			resolvedArgs[i] = arg
		}
	}
	r.errlist.Add(r.file.Position(node.Pos()), fmt.Errorf(format, resolvedArgs...))
}

// ResolveModule walks the AST trying to match every identifier to a declaration in scope. If a match is found, it
// is returned in the symbol table. If a match is not found, the entry is returned in Unresolved. All identifiers
// must be either resolved or unresolved by the end of the walk.
func ResolveModule(module *ast.Module, outer *Scope) error {
	r := &resolver{
		symbols: types.NewSymbolTable(),
		file:    module.File,
	}

	r.ctx.scope = outer
	r.pushScope()
	r.resolveModule(module)
	return r.errlist.Err()
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
	r.declare(x.Id, x, &types.Module{AtomValue: types.AtomValue{V: x.Id.Name}})

	for _, decl := range x.Decls {
		switch decl := decl.(type) {
		case *ast.FuncDecl:
			var argTypes []ast.Type
			for _, arg := range decl.Parameters.List {
				t := r.resolveType(arg.Typ)
				for j := 0; j < len(arg.Names); j++ {
					argTypes = append(argTypes, t)
				}
			}

			var retType ast.Type
			if decl.ReturnType == nil {
				retType = types.Void
			} else {
				retType = r.resolveType(decl.ReturnType)
			}

			fnType := &types.Func{
				Args:   argTypes,
				Return: retType,
			}
			decl.SetType(fnType)
			r.declare(decl.Name, decl, fnType)
		case *ast.ImportDecl:
			moduleName := decl.Path.Value
			alias := decl.Alias
			if alias == nil {
				alias = &ast.Identifier{Name: moduleName}
			}
			r.declare(alias, decl, &types.Module{AtomValue: types.AtomValue{V: moduleName}})
		}
	}

	for _, decl := range x.Decls {
		if fd, ok := decl.(*ast.FuncDecl); ok {
			r.resolveFunc(fd)
		}
	}
}

func (r *resolver) resolveFunc(x *ast.FuncDecl) {
	r.pushScope() // function scope
	defer r.popScope()

	for _, arg := range x.Parameters.List {
		t := r.resolveType(arg.Typ)
		for _, name := range arg.Names {
			r.declare(name, arg, t.Definition)
		}
	}

	// TODO: Declare parameters and manually specified types
	for _, stmt := range x.Statements {
		r.resolveStmt(x, stmt)
	}
}

func (r *resolver) resolveStmt(fn *ast.FuncDecl, stmt ast.Statement) {
	switch s := stmt.(type) {
	case *ast.ExprStatement:
		r.resolveExpr(s.Expression)
	case *ast.ReturnStatement:
		var retType ast.Type = types.Void
		if s.Expression != nil {
			retType = r.resolveExpr(s.Expression)
		}

		fnType, ok := fn.Type().(*types.Func)
		if !ok {
			return // error parsing func types, don't bother checking
		}

		if types.IsAssignable(fnType.Return, retType) == types.Invalid {
			r.error(s.Expression, "cannot return %s from function of return type %s", retType, fnType.Return)
		}
	default:
		panic(fmt.Sprintf("unhandled statement type %T", stmt))
	}
}

func (r *resolver) resolveExpr(expr ast.Expression) (result ast.Type) {
	switch e := expr.(type) {
	case *ast.MatchAssignExpr:
		return r.resolveMatchAssign(e)
	case *ast.Identifier:
		decl := r.lookup(e.Name)
		if decl == nil {
			r.missing(e)
			return types.Invalid
		}
		r.symbols.AddResolved(e, decl)
		e.SetType(decl)
		return decl.Type
	case *ast.IntLiteral:
		e.SetType(types.UntypedInt)
		return types.UntypedInt
	case *ast.FloatLiteral:
		e.SetType(types.UntypedFloat)
		return types.UntypedFloat
	case *ast.StringLiteral:
		e.SetType(types.UntypedString)
		return types.UntypedString
	case *ast.AtomLiteral:
		t := &types.AtomValue{V: e.Value}
		e.SetType(t)
		return t
	case *ast.ListLiteral:
		return r.resolveListExpr(e)
	case *ast.ListType:
		eltType := r.resolveType(e.Elt)
		t := &types.Expr{Definition: &types.List{Elem: eltType}}
		e.SetType(t)
		return t
	case *ast.BinaryExpr:
		return r.resolveBinaryExpr(e)
	case *ast.UnaryExpr:
		return r.resolveUnaryExpr(e)
	case *ast.CallExpr:
		return r.resolveCallExpr(e)
	case *ast.DotExpr:
		return r.resolveDotExpr(e)
	default:
		panic(fmt.Errorf("unsupported expression %T", expr))
	}
}

func (r *resolver) resolveBinaryExpr(expr *ast.BinaryExpr) (result ast.Type) {
	defer func() { expr.SetType(result) }()
	leftT := r.resolveExpr(expr.Left)
	rightT := r.resolveExpr(expr.Right)
	switch expr.Op {
	case token.Plus:
		// Numeric operators OR string concatenation
		if !types.IsString(leftT) && !types.IsNumeric(leftT) {
			r.error(expr.Left, "operator %s not defined on %s (%s)", printer.TokenTable[expr.Op], expr.Left, leftT)
			return types.Invalid
		}
		if !types.IsString(rightT) && !types.IsNumeric(rightT) {
			r.error(expr.Right, "operator %s not defined on %s (%s)", printer.TokenTable[expr.Op], expr.Right, rightT)
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
			r.error(expr.Left, "operator %s not defined on %s (%s)", printer.TokenTable[expr.Op], expr.Left, leftT)
			return types.Invalid
		}
		if !types.IsNumeric(rightT) {
			r.error(expr.Right, "operator %s not defined on %s (%s)", printer.TokenTable[expr.Op], expr.Right, rightT)
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

func (r *resolver) resolveUnaryExpr(expr *ast.UnaryExpr) (result ast.Type) {
	defer func() { expr.SetType(result) }()
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

func (r *resolver) resolveCallExpr(e *ast.CallExpr) (result ast.Type) {
	defer func() { e.SetType(result) }()
	fnType := r.resolveExpr(e.Fun)
	if fnType == types.Any {
		var typs []ast.Type
		for _, arg := range e.Args {
			typs = append(typs, r.resolveExpr(arg)) // resolve, but we can't do any checks about the types
		}
		deduced := &types.Func{Args: typs, Return: types.Any}
		if fn, ok := e.Fun.(types.Node); ok {
			fn.SetType(deduced)
		}
		return types.Any
	}
	switch cl := fnType.(type) {
	case *types.Func:
		for i, arg := range e.Args {
			argType := r.resolveExpr(arg)
			if i >= len(cl.Args) {
				r.error(arg, "too many arguments to func %s (expected %d, got %d)", e.Fun, len(cl.Args), len(e.Args))
				break
			}
			declType := types.Value(cl.Args[i])
			if t := types.IsAssignable(declType, argType); t != declType {
				r.error(arg, "cannot use %s (%s) as %s value in argument to %s", arg, argType, declType, e.Fun)
				continue
			}
		}
		return types.Value(cl.Return)
	case *types.Expr:
		// Type cast expression
		if len(e.Args) != 1 {
			r.error(e, "type cast takes exactly one argument")
			return types.Invalid
		}
		argType := r.resolveExpr(e.Args[0])
		result, err := types.Cast(argType, cl.Definition)
		if err != nil {
			r.error(e.Args[0], "cannot cast %s to %s", argType, cl)
			return types.Invalid
		}
		return result
	default:
		r.error(e.Fun, "cannot call non-function %s (variable of type %s)", e.Fun, fnType)
		return types.Any
	}
}

func (r *resolver) resolveDotExpr(e *ast.DotExpr) (result ast.Type) {
	defer func() { e.SetType(result) }()
	leftT := r.resolveExpr(e.X)
	if mod, ok := leftT.(*types.Module); ok {
		modName := mod.AtomValue.V
		r.ImportModule(modName)
		e.Attr.SetType(types.Any)
		// TODO: Handle importing modules recursively and typechecking them with values defined there
		return types.Any
	}
	return types.Any
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
func (r *resolver) resolveMatchAssign(m *ast.MatchAssignExpr) ast.Type {
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

// resolveType is similar to resolveExpr, except the identifier must refer a type declaration
// rather than a value or other declaration (e.g. module).
func (r *resolver) resolveType(expr ast.Expression) *types.Expr {
	typ := r.resolveExpr(expr)
	switch typ := typ.(type) {
	case *types.Expr:
		return typ
	default:
		r.error(expr, "expected type, found %s", expr)
		return &types.Expr{Definition: types.Invalid}
	}
}

func (r *resolver) resolveListExpr(l *ast.ListLiteral) (result *types.List) {
	defer func() { l.SetType(result) }()
	if len(l.Elts) == 0 {
		return &types.List{Elem: types.Invalid}
	}

	var listType ast.Type
	for _, elem := range l.Elts {
		elemType := r.resolveExpr(elem)
		if listType == nil {
			listType = elemType
		} else if types.IsAssignable(elemType, listType) == elemType {
			listType = elemType
		} else if types.IsAssignable(listType, elemType) == types.Invalid {
			r.error(elem, "cannot use %s (%s) as value in %s list", elem, elemType, listType)
		}
	}
	return &types.List{Elem: listType}
}

func (r *resolver) declare(id *ast.Identifier, decl ast.Node, typ ast.Type) *types.Decl {
	d := &types.Decl{RefersTo: decl, Type: typ}
	r.symbols.AddResolved(id, d)
	id.SetType(d)
	return r.ctx.scope.Insert(id.Name, d)
}

func (r *resolver) lookup(name string) *types.Decl {
	if obj := Universe.Lookup(name); obj != nil {
		return obj
	}
	return r.ctx.scope.Lookup(name)
}

// missing adds a new identifier to be associated in the symbol table as missing.
func (r *resolver) missing(ident *ast.Identifier) {
	r.symbols.MarkUnresolved(ident)
}
