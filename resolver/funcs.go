package resolver

import (
	"github.com/masp/ertylang/ast"
	"github.com/masp/ertylang/types"
)

func (r *resolver) resolveFuncDecl(decl *ast.FuncDecl) {
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
}

func (r *resolver) resolveFuncBody(x *ast.FuncDecl) {
	scope := r.pushScope() // function scope
	defer r.popScope()
	scope.currFn = x

	for _, arg := range x.Parameters.List {
		t := r.resolveType(arg.Typ)
		for _, name := range arg.Names {
			r.declare(name, arg, t.Definition)
		}
	}

	// TODO: Declare parameters and manually specified types
	// TODO: Determine return type from last statement
	for _, stmt := range x.Statements {
		r.resolveStmt(stmt)
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
	case types.Overload:
		// If the enum has multiple functions with different length args, we take the one with the most matching
		// number. If there are multiple with the same number, we throw an error because it's ambiguous.
		best := r.findBestOverload(cl, e)
		if best == nil {
			return types.Any
		}
		r.resolveCallArgs(best, e.Args)
		return types.Value(best.Return)
	case *types.Func:
		if len(e.Args) > len(cl.Args) {
			r.error(e.Args[len(cl.Args)], "too many arguments to func %s (expected %d, got %d)", e.Fun, len(cl.Args), len(e.Args))
			return types.Any
		} else if len(e.Args) < len(cl.Args) {
			r.error(e.Fun, "too few arguments to func %s (expected %d, got %d)", e.Fun, len(cl.Args), len(e.Args))
			return types.Any
		}
		r.resolveCallArgs(cl, e.Args)
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

func (r *resolver) findBestOverload(overload types.Overload, e *ast.CallExpr) *types.Func {
	var best *types.Func
	for _, fn := range overload {
		if len(fn.Args) == len(e.Args) {
			if best != nil {
				r.error(e.Fun, "ambiguous call to overloaded func options '%s' | '%s': both have %d arguments", fn, best, len(e.Args))
			}
			best = fn
		}
	}
	if best == nil {
		r.error(e.Fun, "cannot call any of overloaded funcs '%s': needs %d arguments", overload, len(e.Args))
	}
	return best
}

func (r *resolver) resolveCallArgs(fn *types.Func, args []ast.Expression) {
	for i, arg := range args {
		argType := r.resolveExpr(arg)
		declType := types.Value(fn.Args[i])
		if !types.IsAssignable(declType, argType) {
			r.error(arg, "cannot use %s (%s) as %s value in argument to %s", arg, argType, declType, fn)
			continue
		}
	}
}

func (r *resolver) resolveDotExpr(e *ast.DotExpr) (result ast.Type) {
	defer func() { e.SetType(result) }()
	leftT := r.resolveExpr(e.X)

	switch target := leftT.(type) {
	case *types.Module:
		if target.Imported != nil {
			var t ast.Type
			decls := target.Imported.Lookup(e.Attr.Name)
			if len(decls) == 0 {
				r.error(e.Attr, "undefined: %s", e.Attr.Name)
				return types.Any
			} else if len(decls) == 1 {
				t = decls[0].Type()
			} else {
				t = r.newOverloadedFunc(decls)
			}
			e.Attr.SetType(t)
			return t
		}
		// The module was unable to be resolved, so any means we won't report more errors
		// on it until the user fixes the import.
		return types.Any
	case *types.AtomValue:
		return types.Any // dynamic dispatch using module name
	default:
		r.error(e.Attr, "type %s has no field or method %s", leftT, e.Attr.Name)
		return types.Any
	}
}

func (r *resolver) newOverloadedFunc(cases []ast.Decl) types.Overload {
	var ts types.Overload
	for _, c := range cases {
		t := c.Type()
		if fn, ok := t.(*types.Func); ok {
			ts = append(ts, fn)
		} else {
			panic("overloaded function with non-function case: " + t.String())
		}
	}
	return ts
}
