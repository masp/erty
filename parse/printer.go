package parse

import (
	"fmt"
	"io"

	"github.com/masp/garlang/ast"
)

// printer is a helper struct to print the AST

func NewPrinter(w io.Writer) *printer {
	return &printer{out: w, indentSize: 4}
}

type printer struct {
	out        io.Writer
	currIndent int

	// settings
	indentSize int
}

func (p *printer) indent() {
	p.currIndent++
}

func (p *printer) printIndent() {
	for i := 0; i < p.currIndent*p.indentSize; i++ {
		fmt.Fprint(p.out, " ")
	}
}

func (p *printer) write(format string, args ...interface{}) {
	p.printIndent()
	fmt.Fprintf(p.out, format, args...)
}

func (p *printer) Print(node ast.Node) {
	switch node := node.(type) {
	case *ast.Module:
		p.printModule(node)
	case ast.FuncDecl:
		p.printFuncDecl(node)
	case ast.ConstDecl:
		p.printConstDecl(node)
	case ast.ReturnStatement:
		p.printReturnStatement(node)
	case ast.ExprStatement:
		p.printExprStatement(node)
	case ast.Literal, ast.Identifier:
		p.printTerminal(node)
	case ast.Expression:
		p.printExpr(node)
	default:
		panic(fmt.Sprintf("unknown node type %T", node))
	}
}

func (p *printer) printModule(node *ast.Module) string {
	p.write("Module {\n")
	p.indent()
	p.write("Name: %s\n", node.Name)
	for _, decl := range node.Constants {
		p.Print(decl)
	}
	for _, decl := range node.Functions {
		p.Print(decl)
	}
	p.currIndent--
	p.write("}\n")
	return ""
}

func (p *printer) printFuncDecl(node ast.FuncDecl) string {
	p.write("FuncDecl {\n")
	p.indent()
	p.write("Name: %s\n", node.Name)
	p.write("Exported: %v\n", node.Exported)
	p.write("Parameters: [\n")
	p.indent()
	for _, param := range node.Parameters {
		p.printIndent()
		p.Print(param)
	}
	p.currIndent--
	p.write("]\n")
	p.write("Statements: [\n")
	p.indent()
	for _, stmt := range node.Statements {
		p.Print(stmt)
	}
	p.currIndent--
	p.write("]\n")
	p.currIndent--
	p.write("}\n")
	return ""
}

func (p *printer) printConstDecl(node ast.ConstDecl) {
	p.write("ConstDecl {\n")
	p.indent()
	p.write("Identifier: %s\n", node.Identifier)
	p.write("Value: ")
	p.Print(node.Value)
	p.currIndent--
	p.write("}\n")
}

func (p *printer) printReturnStatement(node ast.ReturnStatement) {
	p.write("ReturnStatement {\n")
	p.indent()
	p.write("Expression: ")
	p.Print(node.Expression)
	p.currIndent--
	p.write("}\n")
}

func (p *printer) printExprStatement(node ast.ExprStatement) {
	p.write("ExprStatement ")
	p.Print(node.Expression)
}

func (p *printer) printExpr(node ast.Expression) {
	fmt.Fprint(p.out, "{\n")
	p.indent()
	switch node := node.(type) {
	case ast.BinaryExpr:
		p.printBinaryExpr(node)
	case ast.UnaryExpr:
		p.printUnaryExpr(node)
	case ast.AssignExpr:
		p.printAssignExpr(node)
	case ast.MatchAssignExpr:
		p.printColonAssignExpr(node)
	case ast.CallExpr:
		p.printCallExpr(node)
	case ast.DotExpr:
		p.printDotExpr(node)
	default:
		panic(fmt.Sprintf("unknown node type %T", node))
	}
	p.currIndent--
	p.write("}\n")
}

func (p *printer) printAssignExpr(node ast.AssignExpr) {
	p.write("AssignExpr {\n")
	p.indent()
	p.write("Left: %s\n", node.Left.Value)
	p.write("Right: ")
	p.Print(node.Right)
	p.currIndent--
	p.write("}\n")
}

func (p *printer) printColonAssignExpr(node ast.MatchAssignExpr) {
	p.write("ColonAssignExpr {\n")
	p.indent()
	p.write("Left: ")
	p.Print(node.Left)
	p.write("Right: ")
	p.Print(node.Right)
	p.currIndent--
	p.write("}\n")
}

func (p *printer) printTerminal(node ast.Node) {
	var val string
	switch node := node.(type) {
	case ast.StringLiteral:
		val = p.printStringLiteral(node)
	case ast.IntLiteral:
		val = p.printIntLiteral(node)
	case ast.FloatLiteral:
		val = p.printFloatLiteral(node)
	case ast.Identifier:
		val = p.printIdentifier(node)
	default:
		panic(fmt.Sprintf("unknown node type %T", node))
	}
	fmt.Fprintf(p.out, "%s\n", val)
}

func (p *printer) printUnaryExpr(node ast.UnaryExpr) {
	p.write("Unary (%s) {\n", node.Operator)
	p.indent()

	p.write("Expression: ")
	p.Print(node.Right)

	p.currIndent--
	p.write("}\n")
}

func (p *printer) printBinaryExpr(node ast.BinaryExpr) {
	p.write("%s {\n", node.Operator)
	p.indent()

	p.write("Left: ")
	p.Print(node.Left)

	p.write("Right: ")
	p.Print(node.Right)

	p.currIndent--
	p.write("}\n")
}

func (p *printer) printCallExpr(node ast.CallExpr) {
	p.write("CallExpr {\n")
	p.indent()

	p.write("Callee: ")
	p.Print(node.Callee)

	p.write("Arguments: [\n")
	p.indent()
	for _, arg := range node.Arguments {
		p.printIndent()
		p.Print(arg)
	}
	p.currIndent--
	p.write("]\n")

	p.currIndent--
	p.write("}\n")
}

func (p *printer) printDotExpr(node ast.DotExpr) {
	p.write("DotExpr {\n")
	p.indent()

	p.write("Target: ")
	p.Print(node.Target)
	p.write("Attribute: %s\n", node.Attribute.Value)

	p.currIndent--
	p.write("}\n")
}

func (p *printer) printIdentifier(node ast.Identifier) string {
	return fmt.Sprintf("Identifier { Name: %s }", node.Name.Value)
}

func (p *printer) printStringLiteral(node ast.StringLiteral) string {
	return fmt.Sprintf("StringLiteral { Value: %s }", node.Value)
}

func (p *printer) printIntLiteral(node ast.IntLiteral) string {
	return fmt.Sprintf("IntLiteral { Value: %d }", node.Value)
}

func (p *printer) printFloatLiteral(node ast.FloatLiteral) string {
	return fmt.Sprintf("FloatLiteral { Value: %f }", node.Value)
}
