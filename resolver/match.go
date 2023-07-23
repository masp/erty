package resolver

import (
	"github.com/masp/ertylang/ast"
	"github.com/masp/ertylang/types"
)

func (r *resolver) resolveMatch(match *ast.Match) ast.Type {
	// Resolve the pattern first to determine types in that expression which we then will
	// bind to values to determine type in each clause.
	valueT := r.resolveExpr(match.Value)

	caseTs := make([]ast.Type, 0, len(match.Cases))
	for _, arm := range match.Cases {
		caseT := r.resolveCase(match, valueT, arm)
		if caseT == types.Void {
			continue
		}
		// Resolve the case pattern and unify it with the value expression.
		caseTs = append(caseTs, caseT)
	}
	return types.Merge(caseTs...)
}

func (r *resolver) resolveCase(match *ast.Match, valueT ast.Type, arm *ast.Case) ast.Type {
	r.pushScope()
	defer r.popScope()

	patternT := r.resolveExpr(arm.Pattern)
	unified := types.Unify(patternT, valueT)
	if unified == types.Invalid {
		r.error(arm.Pattern, "cannot unify value %s (%s) with case pattern %s (%s)", match.Value, valueT, arm.Pattern, patternT)
		return types.Invalid
	}

	var bodyT ast.Type
	for _, stmt := range arm.Body {
		bodyT = r.resolveStmt(stmt)
	}
	return bodyT
}

func (r *resolver) resolveField(e *ast.Field) ast.Type {
	// If an identifier is used with a type, we just resolve the type and return it while
	// adding it to the scope.
	if len(e.Names) != 1 {
		panic("field should not have more than 1 name in pattern")
	}
	typ := r.resolveType(e.Typ)
	return r.declare(e.Names[0], e, typ)
}
