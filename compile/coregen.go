package compile

import (
	"fmt"
	"io"
	"strings"

	"github.com/masp/garlang/ast"
)

// gen phase generates the core erlang code from the AST.
type gen struct {
	Output                 io.Writer
	indentSize, currIndent int
}

func (c *gen) indent() {
	c.currIndent++
}

func (c *gen) dedent() {
	c.currIndent--
}

func (c *gen) emitln() {
	c.Output.Write([]byte{'\n'})
	io.WriteString(c.Output, strings.Repeat(" ", c.indentSize*c.currIndent))
}

func (c *gen) emitf(format string, args ...interface{}) {
	fmt.Fprintf(c.Output, format, args...)
}

func (c *gen) EmitModule(mod *ast.Module) {
	c.emitf("module '%s' [", mod.Name)
	i := 0
	for _, fn := range mod.Functions {
		if !fn.IsPublic() {
			continue
		}
		if i > 0 {
			c.emitf(",")
		}
		i++
		c.emitf("'%s'/%d", fn.Name, len(fn.Parameters))
	}
	c.emitf("]")
	c.emitln()
	for _, fn := range mod.Functions {
		c.EmitFunction(fn)
	}
	c.emitf("end")
}

func (c *gen) EmitFunction(fn ast.FuncDecl) {
	c.emitFnHeader(fn)
	c.emitFn(fn)
	c.dedent()
	c.emitln()
}

func (c *gen) emitFnHeader(fn ast.FuncDecl) {
	c.emitf("'%s'/%d =", fn.Name, len(fn.Parameters))
	c.indent()
	c.emitln()
}

func (c *gen) emitFn(fn ast.FuncDecl) {
	c.emitf("(fun (")
	for i, param := range fn.Parameters {
		if i > 0 {
			c.emitf(",")
		}
		c.emitf("%s", param)
	}
	c.emitf(") ->")

	for _, stmt := range fn.Statements {
		c.emitln()
		c.emitStmt(stmt)
	}
	c.emitf(")")
}

func (c *gen) emitStmt(stmt ast.Statement) {
	switch stmt := stmt.(type) {
	case ast.ExprStatement:
		c.emitExpr(stmt.Expression)
	case ast.ReturnStatement:
		c.emitReturn(stmt)
	default:
		panic(fmt.Sprintf("unknown statement type %T", stmt))
	}
}

func (c *gen) emitAssignment(stmt ast.AssignExpr) {
	// TODO
}

func (c *gen) emitReturn(stmt ast.ReturnStatement) {
	// Erlang doesn't use return statement, just final value
	c.emitExpr(stmt.Expression)
}

func (c *gen) emitExpr(expr ast.Expression) {
	switch expr := expr.(type) {
	case ast.Literal:
		c.emitLiteral(expr)
	case ast.Identifier:
		c.emitIdentifier(expr)
	case ast.AssignExpr:
		c.emitAssignment(expr)
	default:
		panic(fmt.Sprintf("unknown expression type %T", expr))
	}
}

func (c *gen) emitLiteral(lit ast.Literal) {
	switch lit := lit.(type) {
	case ast.IntLiteral:
		c.emitf("%d", lit.Value)
	case ast.FloatLiteral:
		c.emitf("%f", lit.Value)
	case ast.StringLiteral:
		c.emitf("<<\"%s\">>", lit.Value)
	case ast.AtomLiteral:
		c.emitf("'%s'", lit.Value)
	default:
		panic(fmt.Sprintf("unknown literal type %T", lit))
	}
}

func (c *gen) emitIdentifier(id ast.Identifier) {
	c.emitf("%s", id.Name)
}
