package ast

import (
	"github.com/masp/garlang/lexer"
	"github.com/masp/garlang/token"
)

type Node interface {
	isNode()
	Pos() token.Pos
	End() token.Pos
}

type Module struct {
	File  *token.File
	Id    *Identifier
	Decls []Decl

	Imports []*ImportDecl
	Scope   *Scope // this module only
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

type ImportDecl struct {
	Import token.Pos      // `import` keyword
	Alias  *Identifier    // name to import (default to last element of path). Can be nil.
	Path   *StringLiteral // value of import
}

func (i *ImportDecl) isDeclaration() {}
func (i *ImportDecl) isNode()        {}
func (i *ImportDecl) Pos() token.Pos {
	return i.Import
}
func (i *ImportDecl) End() token.Pos {
	return i.Path.End()
}

// TypeDecl defines a new type, and looks like `[export] type <name> <definition>`
type TypeDecl struct {
	Type token.Pos // `type` keyword

	Name       *Identifier // the new type name
	Definition Expression  // the type value
}

func (t *TypeDecl) isDeclaration() {}
func (t *TypeDecl) isNode()        {}
func (t *TypeDecl) Pos() token.Pos {
	return t.Type
}
func (t *TypeDecl) End() token.Pos {
	return t.Definition.End()
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
	LeftBrace  token.Pos // `{` and `}` token
	RightBrace token.Pos

	Name       *Identifier   // function name
	Parameters []*Identifier // function parameters
	Statements []Statement
}

func (f *FuncDecl) IsPublic() bool {
	return f.Name.Name[0] != '_'
}

func (f *FuncDecl) isDeclaration() {}
func (f *FuncDecl) isNode()        {}
func (f *FuncDecl) Pos() token.Pos {
	return f.Func
}
func (f *FuncDecl) End() token.Pos {
	return f.RightBrace + 1
}

type BadDecl struct {
	From, To token.Pos
}

func (b *BadDecl) isDeclaration() {}
func (b *BadDecl) isNode()        {}
func (b *BadDecl) Pos() token.Pos {
	return b.From
}
func (b *BadDecl) End() token.Pos {
	return b.To
}

type Statement interface {
	Node
	isStatement()
}

type BadStmt struct {
	From, To token.Pos
}

func (b *BadStmt) isStatement() {}
func (b *BadStmt) isNode()      {}
func (b *BadStmt) Pos() token.Pos {
	return b.From
}
func (b *BadStmt) End() token.Pos {
	return b.To
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

type BadExpr struct {
	From, To token.Pos
}

func (b *BadExpr) isExpression() {}
func (b *BadExpr) isNode()       {}
func (b *BadExpr) Pos() token.Pos {
	return b.From
}
func (b *BadExpr) End() token.Pos {
	return b.To
}

// A Field represents a Field declaration list in a struct type,
// a method list in an interface type, or a parameter/result declaration
// in a signature.
// Field.Names is nil for unnamed parameters (parameter lists which only contain types)
type Field struct {
	Names []*Identifier // field/method/(type) parameter names; or nil
	Type  Expression    // field/method/parameter type; or nil
}

func (f *Field) Pos() token.Pos {
	if len(f.Names) > 0 {
		return f.Names[0].Pos()
	}
	if f.Type != nil {
		return f.Type.Pos()
	}
	return token.NoPos
}

func (f *Field) End() token.Pos {
	if f.Type != nil {
		return f.Type.End()
	}
	if len(f.Names) > 0 {
		return f.Names[len(f.Names)-1].End()
	}
	return token.NoPos
}

// A FieldList represents a list of Fields, enclosed by parentheses,
// curly braces, or square brackets.
type FieldList struct {
	Opening token.Pos // position of opening parenthesis/brace/bracket, if any
	List    []*Field  // field list; or nil
	Closing token.Pos // position of closing parenthesis/brace/bracket, if any
}

func (f *FieldList) Pos() token.Pos {
	if f.Opening.IsValid() {
		return f.Opening
	}
	// the list should not be empty in this case;
	// be conservative and guard against bad ASTs
	if len(f.List) > 0 {
		return f.List[0].Pos()
	}
	return token.NoPos
}

func (f *FieldList) End() token.Pos {
	if f.Closing.IsValid() {
		return f.Closing + 1
	}
	// the list should not be empty in this case;
	// be conservative and guard against bad ASTs
	if n := len(f.List); n > 0 {
		return f.List[n-1].End()
	}
	return token.NoPos
}

// NumFields returns the number of parameters or struct fields represented by a FieldList.
func (f *FieldList) NumFields() int {
	n := 0
	if f != nil {
		for _, g := range f.List {
			m := len(g.Names)
			if m == 0 {
				m = 1
			}
			n += m
		}
	}
	return n
}

type TupleType struct {
	Tuple token.Pos  // `tuple` keyword
	Elts  *FieldList // types for elements
}

func (t *TupleType) isExpression() {}
func (t *TupleType) isNode()       {}
func (t *TupleType) Pos() token.Pos {
	return t.Tuple
}
func (t *TupleType) End() token.Pos {
	return t.Elts.End()
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

type KVExpr struct {
	Key, Value Expression
	Colon      token.Pos
	Comma      token.Pos // Invalid if no comma after element
}

func (m *KVExpr) isNode() {}
func (m *KVExpr) Pos() token.Pos {
	return m.Key.Pos()
}
func (m *KVExpr) End() token.Pos {
	if m.Comma.IsValid() {
		return m.Comma + 1
	}
	return m.Value.End()
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
func (i *Identifier) IsPublic() bool {
	return i.Name[0] != '_'
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
