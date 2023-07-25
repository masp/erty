package compiler

import (
	"github.com/masp/ertylang/ast"
	"github.com/masp/ertylang/core"
)

func (c *Compiler) compileMatch(match *ast.Match) core.Expr {
	result := &core.Case{}
	result.E = c.compileExpr(match.Value)
	for _, arm := range match.Cases {
		result.Clauses = append(result.Clauses, c.compileCaseClause(match, arm))
	}
	return result
}

func (c *Compiler) compileCaseClause(match *ast.Match, arm *ast.Case) *core.Clause {
	result := &core.Clause{}
	c.push()
	defer c.pop()
	result.Pattern = c.compileExpr(arm.Pattern)
	result.When = &core.Atom{Value: "true"} // TODO: Add more complicated whens to capture guards
	result.Body = c.compileStatements(arm.Body)
	return result
}

func (c *Compiler) compileField(field *ast.Field) core.Expr {
	if len(field.Names) != 1 {
		panic("only 1 field name allowed")
	}
	return c.declare(field.Names[0])
}
