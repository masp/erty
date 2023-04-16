package ast

import (
	"github.com/masp/garlang/lexer"
)

// These are the list of all possible AST nodes. The AST nodes follow these rules:
// - A program is a sequence of declarations.
// - A block is either a function block or a constant block.
// - A constant block is the const keyword followed by an identifier and a value.
// - A function declaration is a function name followed by a sequence of parameters enclosed in parentheses, followed by a sequence of statements enclosed in braces.
// - A statement is either an assignment statement or an expression statement.
// - An assignment statement assigns a value or expression to an identifier.
// - A value can be either a string enclosed in double quotes or a decimal number.
// - An expression statement is an expression that evaluates to a value.
//
// The following are examples of expressions:
// - Addition: 1 + 2 (<number> + <number>)
// - Subtraction: 1 - 2 (<number> - <number>)
// - Multiplication: 1 * 2 (<number> * <number>)
// - Division: 1 / 2 (<number> / <number>)
// - Modulus: 1 % 2 (<number> % <number>)
// - Function call: add(1, 2) (<identifier> (<expression>, <expression>))
// - Variable: x (<identifier>)
// - String: "hello" (<string>)
// - Number: 123 (<number>)

type Node interface {
	isNode()
}

type Module struct {
	Name      string
	Functions []FuncDecl
	Constants []ConstDecl
}

func (p *Module) isNode() {}

type Decl interface {
	isDeclaration()
}

type ConstDecl struct {
	Identifier string
	Value      Literal
}

func (c ConstDecl) isDeclaration() {}
func (c ConstDecl) isNode()        {}

type FuncDecl struct {
	Name       string
	Parameters []Identifier
	Statements []Statement

	Exported bool
}

func (f FuncDecl) IsPublic() bool {
	return f.Exported
}

func (f FuncDecl) isDeclaration() {}
func (f FuncDecl) isNode()        {}

type Statement interface {
	Node
	isStatement()
}

type ExprStatement struct {
	Expression
}

func (e ExprStatement) isStatement() {}
func (e ExprStatement) isNode()      {}

type ReturnStatement struct {
	Expression Expression
}

func (e ReturnStatement) isStatement() {}
func (e ReturnStatement) isNode()      {}

type Expression interface {
	Node
	isExpression()
}

type CallExpr struct {
	Callee    Expression
	Arguments []Expression
}

func (u CallExpr) isExpression() {}
func (u CallExpr) isNode()       {}

type DotExpr struct {
	Target    Expression
	Attribute lexer.Token
}

func (u DotExpr) isExpression() {}
func (u DotExpr) isNode()       {}

type UnaryExpr struct {
	Operator string
	Right    Expression
}

func (u UnaryExpr) isExpression() {}
func (u UnaryExpr) isNode()       {}

type BinaryExpr struct {
	Left     Expression
	Operator string
	Right    Expression
}

func (b BinaryExpr) isExpression() {}
func (b BinaryExpr) isNode()       {}

type Literal interface {
	Node
	Expression
	isLiteral()
}

type StringLiteral struct {
	Value string
}

func (s StringLiteral) isExpression() {}
func (s StringLiteral) isLiteral()    {}
func (s StringLiteral) isNode()       {}

type AtomLiteral struct {
	Value string
}

func (s AtomLiteral) isExpression() {}
func (s AtomLiteral) isLiteral()    {}
func (s AtomLiteral) isNode()       {}

type IntLiteral struct {
	Value int64
}

func (n IntLiteral) isExpression() {}
func (s IntLiteral) isLiteral()    {}
func (s IntLiteral) isNode()       {}

type FloatLiteral struct {
	Value float64
}

func (n FloatLiteral) isExpression() {}
func (s FloatLiteral) isLiteral()    {}
func (s FloatLiteral) isNode()       {}

type Identifier struct {
	Name lexer.Token
}

func (i Identifier) isExpression() {}
func (i Identifier) isNode()       {}

type ParenExpr struct {
	Expression
}

func (p ParenExpr) isExpression() {}
func (p ParenExpr) isNode()       {}

type AssignExpr struct { // '='
	Left  lexer.Token // must be identifier
	Right Expression
}

func (a AssignExpr) isExpression() {}
func (a AssignExpr) isNode()       {}

type MatchAssignExpr struct { // ':='
	Left  Expression
	Right Expression
}

func (a MatchAssignExpr) isExpression() {}
func (a MatchAssignExpr) isNode()       {}
