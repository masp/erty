package ast

import (
	"github.com/masp/garlang/lexer"
	"github.com/masp/garlang/token"
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
	Pos() token.Pos
	End() token.Pos
}

type Module struct {
	File  *token.File
	Id    *Identifier
	Decls []Decl
}

func (p *Module) isNode() {}
func (p *Module) Pos() token.Pos {
	return p.File.Pos(0)
}
func (p *Module) End() token.Pos {
	return p.File.Pos(p.File.Size)
}

type Decl interface {
	Node
	isDeclaration()
}

type ConstDecl struct {
	Const      token.Pos   // `const` keyword
	Identifier *Identifier // left hand of assignment
	Value      Literal     // right hand of assignment
}

func (c *ConstDecl) isDeclaration() {}
func (c *ConstDecl) isNode()        {}
func (c *ConstDecl) Pos() token.Pos {
	return c.Const
}
func (c *ConstDecl) End() token.Pos {
	return c.Value.End()
}

type FuncDecl struct {
	Func       token.Pos // `func` keyword
	Export     token.Pos // IsValid() if `export` keyword exists
	LeftBrace  token.Pos // `{` and `}` token
	RightBrace token.Pos

	Name       *Identifier   // function name
	Parameters []*Identifier // function parameters
	Statements []Statement
}

func (f *FuncDecl) IsPublic() bool {
	return f.Export.IsValid()
}

func (f *FuncDecl) isDeclaration() {}
func (f *FuncDecl) isNode()        {}
func (f *FuncDecl) Pos() token.Pos {
	if f.Export.IsValid() {
		return f.Export
	}
	return f.Func
}
func (f *FuncDecl) End() token.Pos {
	return f.RightBrace + 1
}

type Statement interface {
	Node
	isStatement()
}

type ExprStatement struct {
	Expression
}

func (e *ExprStatement) isStatement() {}
func (e *ExprStatement) isNode()      {}

type ReturnStatement struct {
	Return     token.Pos // `return` keyword
	Expression Expression
}

func (e *ReturnStatement) isStatement() {}
func (e *ReturnStatement) isNode()      {}
func (e *ReturnStatement) Pos() token.Pos {
	return e.Return
}
func (e *ReturnStatement) End() token.Pos {
	return e.Expression.End()
}

type Expression interface {
	Node
	isExpression()
}

type CallExpr struct {
	Callee    Expression
	Arguments []Expression

	LeftParen, RightParen token.Pos
}

func (u *CallExpr) isExpression()  {}
func (u *CallExpr) isNode()        {}
func (u *CallExpr) Pos() token.Pos { return u.Callee.Pos() }
func (u *CallExpr) End() token.Pos { return u.RightParen + 1 }

type DotExpr struct {
	Target    Expression
	Dot       token.Pos
	Attribute *Identifier
}

func (u *DotExpr) isExpression() {}
func (u *DotExpr) isNode()       {}
func (u *DotExpr) Pos() token.Pos {
	return u.Target.Pos()
}
func (u *DotExpr) End() token.Pos {
	return u.Attribute.End()
}

type UnaryExpr struct {
	Op    token.Type
	OpPos token.Pos
	Right Expression
}

func (u *UnaryExpr) isExpression() {}
func (u *UnaryExpr) isNode()       {}
func (u *UnaryExpr) Pos() token.Pos {
	return u.OpPos
}
func (u *UnaryExpr) End() token.Pos {
	return u.Right.End()
}

type BinaryExpr struct {
	Left  Expression
	OpPos token.Pos
	Op    token.Type
	Right Expression
}

func (b *BinaryExpr) isExpression() {}
func (b *BinaryExpr) isNode()       {}
func (b *BinaryExpr) Pos() token.Pos {
	return b.Left.Pos()
}
func (b *BinaryExpr) End() token.Pos {
	return b.Right.End()
}

type Literal interface {
	Node
	Expression
	isLiteral()
}

type StringLiteral struct {
	QuotePos token.Pos
	Value    string
}

func (s *StringLiteral) isExpression() {}
func (s *StringLiteral) isLiteral()    {}
func (s *StringLiteral) isNode()       {}
func (s *StringLiteral) Pos() token.Pos {
	return s.QuotePos
}
func (s *StringLiteral) End() token.Pos {
	return s.QuotePos + token.Pos(len(s.Value)) + 2 // +2 for quotes
}

type AtomLiteral struct {
	QuotePos token.Pos
	Value    string
}

func (s *AtomLiteral) isExpression() {}
func (s *AtomLiteral) isLiteral()    {}
func (s *AtomLiteral) isNode()       {}
func (s *AtomLiteral) Pos() token.Pos {
	return s.QuotePos
}
func (s *AtomLiteral) End() token.Pos {
	return s.QuotePos + token.Pos(len(s.Value)) + 2 // +2 for quotes
}

type IntLiteral struct {
	IntPos token.Pos // position of the first digit
	Lit    string    // raw string, e.g. "12"
	Value  int64     // parsed value
}

func (n *IntLiteral) isExpression() {}
func (s *IntLiteral) isLiteral()    {}
func (s *IntLiteral) isNode()       {}
func (s *IntLiteral) Pos() token.Pos {
	return s.IntPos
}
func (s *IntLiteral) End() token.Pos {
	return s.IntPos + token.Pos(len(s.Lit))
}

type FloatLiteral struct {
	FloatPos token.Pos // position of the first digit
	Lit      string    // raw string, e.g. "12.3"
	Value    float64   // parsed value
}

func (s *FloatLiteral) isExpression() {}
func (s *FloatLiteral) isLiteral()    {}
func (s *FloatLiteral) isNode()       {}
func (s *FloatLiteral) Pos() token.Pos {
	return s.FloatPos
}
func (s *FloatLiteral) End() token.Pos {
	return s.FloatPos + token.Pos(len(s.Lit))
}

func NewIdent(tok lexer.Token) *Identifier {
	if tok.Type != token.Identifier {
		panic("not an identifier")
	}
	return &Identifier{
		NamePos: tok.Pos,
		Name:    tok.Lit,
	}
}

type Identifier struct {
	NamePos token.Pos
	Name    string
}

func (i *Identifier) isExpression() {}
func (i *Identifier) isNode()       {}
func (i *Identifier) Pos() token.Pos {
	return i.NamePos
}
func (i *Identifier) End() token.Pos {
	return i.NamePos + token.Pos(len(i.Name))
}

type ParenExpr struct {
	LParen, RParen token.Pos // '(' and ')' positions
	Expression
}

func (p *ParenExpr) isExpression() {}
func (p *ParenExpr) isNode()       {}
func (p *ParenExpr) Pos() token.Pos {
	return p.LParen
}
func (p *ParenExpr) End() token.Pos {
	return p.RParen + 1
}

type AssignExpr struct { // '='
	Left   *Identifier
	Equals token.Pos
	Right  Expression
}

func (a *AssignExpr) isExpression() {}
func (a *AssignExpr) isNode()       {}
func (a *AssignExpr) Pos() token.Pos {
	return a.Left.Pos()
}
func (a *AssignExpr) End() token.Pos {
	return a.Right.End()
}

type MatchAssignExpr struct { // ':='
	Left   Expression
	Equals token.Pos
	Right  Expression
}

func (a *MatchAssignExpr) isExpression() {}
func (a *MatchAssignExpr) isNode()       {}
func (a *MatchAssignExpr) Pos() token.Pos {
	return a.Left.Pos()
}
func (a *MatchAssignExpr) End() token.Pos {
	return a.Right.End()
}
