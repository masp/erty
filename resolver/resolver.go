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
	ctx context

	file     *token.File
	errlist  token.ErrorList
	importer Importer
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

type Config struct {
	// Outer contains symbols that can be referenced globally (e.g. abs). If nil, no
	// globals defined.
	Outer *Scope

	// Importer is called to resolve import paths whenever an import declaration is found.
	// If nil, all imports cause ErrNoImporter to be returned.
	Importer Importer
}

// ResolveModule walks the AST trying to match every identifier to a declaration in scope. If a match is found, it
// is returned in the symbol table. If a match is not found, the entry is returned in Unresolved. All identifiers
// must be either resolved or unresolved by the end of the walk.
func ResolveModule(module *ast.Module, cfg *Config) error {
	r := &resolver{
		file: module.File,
	}

	if cfg != nil {
		r.ctx.scope = cfg.Outer
		r.importer = cfg.Importer
	}
	if r.ctx.scope == nil {
		r.ctx.scope = Universe
	}
	r.pushScope()
	r.resolveModule(module)
	return r.errlist.Err()
}

func ResolveExpr(file *token.File, expr ast.Expression, cfg *Config) error {
	r := &resolver{
		file: file,
	}

	if cfg != nil {
		r.ctx.scope = cfg.Outer
		r.importer = cfg.Importer
	}
	if r.ctx.scope == nil {
		r.ctx.scope = Universe
	}
	r.pushScope()
	r.resolveExpr(expr)
	return r.errlist.Err()
}

// resolve recursively traverses the expression and declares all identifiers the given scope as well as defining
// subscopes for each block found.
func (r *resolver) resolveModule(x *ast.Module) {
	// Resolve the module name, but do not add to any scope.
	x.Id.SetType(&types.Decl{RefersTo: x, Type: &types.Module{AtomValue: types.AtomValue{V: x.Id.Name}}})
	// r.declare(x.Id, x, &types.Module{AtomValue: types.AtomValue{V: x.Id.Name}})

	for _, decl := range x.Decls {
		switch decl := decl.(type) {
		case *ast.FuncDecl:
			r.resolveFuncDecl(decl)
		case *ast.ImportDecl:
			r.resolveImport(decl)
		}
	}

	for _, decl := range x.Decls {
		if fd, ok := decl.(*ast.FuncDecl); ok {
			r.resolveFuncBody(fd)
		}
	}
}

func (r *resolver) resolveStmt(stmt ast.Statement) ast.Type {
	fn := r.ctx.scope.CurrentFunc()

	switch s := stmt.(type) {
	case *ast.ExprStatement:
		return r.resolveExpr(s.Expression)
	case *ast.ReturnStatement:
		var retType ast.Type = types.Void
		if s.Expression != nil {
			retType = r.resolveExpr(s.Expression)
		}

		fnType, ok := fn.Type().(*types.Func)
		if !ok {
			return types.Void // error parsing func types, don't bother checking
		}

		if !types.IsAssignable(fnType.Return, retType) {
			r.error(s.Expression, "cannot return %s from function of return type %s", retType, fnType.Return)
		}
		return types.Void
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
			e.SetType(types.Invalid)
			r.error(e, "undefined: %s", e.Name)
			return types.Invalid
		}
		e.SetType(decl)
		return decl.Type
	case *ast.Field:
		return r.resolveField(e)
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
	case *ast.Match:
		return r.resolveMatch(e)
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
			r.error(expr.Right, "operator %s not defined on %s (%s)", printer.TokenTable[expr.Op], expr.Right, t)
			return types.Invalid
		}
		return t
	case token.Bang:
		t := r.resolveExpr(expr.Right)
		if types.Bool.Intersect(t) == types.Invalid {
			r.error(expr.Right, "operator %s not defined on %s (%s)", printer.TokenTable[expr.Op], expr.Right, t)
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
		} else if types.IsAssignable(elemType, listType) {
			// Handles cases like [1, A int, 3] where a following elements matches an earlier element's type
			// but the previous element is untyped. We want to use the typed element's type instead of having
			// the list be untyped as well.
			listType = elemType
		} else if !types.IsAssignable(listType, elemType) {
			r.error(elem, "cannot use %s (%s) as value in %s list", elem, elemType, listType)
		}
	}
	return &types.List{Elem: listType}
}

func (r *resolver) declare(id *ast.Identifier, decl ast.Node, typ ast.Type) *types.Decl {
	newDecl := &types.Decl{RefersTo: decl, Type: types.Value(typ)}
	id.SetType(newDecl)

	if r.ctx.scope.Level == ScopeModule {
		// Do not allow overriding declarations in the same scope *unless* it's a function overload
		existing := r.lookup(id.Name)
		newFn, isFunc := typ.(*types.Func)
		if existing != nil {
			if isFunc {
				// Override the declaration with an enum of overloaded functions
				newDecl = r.declareOverload(decl.(*ast.FuncDecl), newFn, existing)
			} else {
				for _, node := range []ast.Node{existing.RefersTo, decl} {
					r.error(node, "cannot redeclare '%s' in the same module", id.Name, typ)
				}
				return existing
			}
		}
	} // otherwise, the most recent declaration overrides the previous one
	return r.ctx.scope.Insert(id.Name, newDecl)
}

// declareOverload handles if there is an existing declaration of a function with the same name as decl
// but different number of arguments. We store function overloads as enums of declarations of funcs,
// and then when the type is applied, we automatically select the correct overload.
func (r *resolver) declareOverload(decl *ast.FuncDecl, newFn *types.Func, existing *types.Decl) *types.Decl {
	switch existingT := existing.Type.(type) {
	case *types.Func:
		// Just one function has been defined previously
		if len(existingT.Args) == len(newFn.Args) {
			for _, node := range []ast.Node{existing.RefersTo, decl} {
				r.error(node, "cannot redeclare func '%s/%d' in module", decl.Name, len(newFn.Args))
			}
			return existing
		}
		return &types.Decl{RefersTo: decl, Type: types.Overload{
			existingT,
			newFn,
		}}
	case types.Overload:
		for _, overload := range existingT {
			if len(overload.Args) == len(newFn.Args) {
				for _, node := range []ast.Node{existing.RefersTo, decl} {
					r.error(node, "cannot redeclare func '%s/%d' in module", decl.Name, len(newFn.Args))
				}
				return existing
			}
		}
	default:
		for _, node := range []ast.Node{existing.RefersTo, decl} {
			r.error(node, "cannot redeclare '%s' in the same module", decl.Name.Name)
		}
	}
	return existing
}

func (r *resolver) lookup(name string) *types.Decl {
	return r.ctx.scope.Lookup(name)
}
