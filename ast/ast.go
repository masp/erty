package ast

import (
	"path"
	"strings"

	"github.com/masp/ertylang/lexer"
	"github.com/masp/ertylang/token"
)

type Node interface {
	isNode()
	Pos() token.Pos
	End() token.Pos
}

// A Type represents any type in the language. Some nodes in the AST can be associated
// with a type. Mostly expressions and declarations.
type Type interface {
	// Underlying returns the underlying type of a type. An underlying type must be one of the built in types.
	Underlying() Type

	// String returns a string representation of a type.
	String() string
}

// noType marks a node as not supporting any attached types.
type noType struct{}

func (n *noType) SetType(t Type) { panic("not supported") }
func (n *noType) Type() Type     { return nil }

type typeNode struct{ typ Type }

func (y *typeNode) SetType(t Type) { y.typ = t }
func (y *typeNode) Type() Type     { return y.typ }

// ----------------------------------------------------------------------------
// Comments

// A Comment node represents a single //-style or /*-style comment.
//
// The Text field contains the comment text without carriage returns (\r) that
// may have been present in the source. Because a comment's end position is
// computed using len(Text), the position reported by End() does not match the
// true source end position for comments containing carriage returns.
type Comment struct {
	noType
	Slash token.Pos // position of "/" starting the comment
	Text  string    // comment text (excluding '\n' for //-style comments)
}

func (c *Comment) Pos() token.Pos { return c.Slash }
func (c *Comment) End() token.Pos { return token.Pos(int(c.Slash) + len(c.Text)) }

// A CommentGroup represents a sequence of comments
// with no other tokens and no empty lines between.
type CommentGroup struct {
	noType
	List []*Comment // len(List) > 0
}

func (g *CommentGroup) Pos() token.Pos { return g.List[0].Pos() }
func (g *CommentGroup) End() token.Pos { return g.List[len(g.List)-1].End() }

func isWhitespace(ch byte) bool { return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' }

func stripTrailingWhitespace(s string) string {
	i := len(s)
	for i > 0 && isWhitespace(s[i-1]) {
		i--
	}
	return s[0:i]
}

// Text returns the text of the comment.
// Comment markers (//, /*, and */), the first space of a line comment, and
// leading and trailing empty lines are removed.
// Comment directives like "//line" are also removed.
// Multiple empty lines are reduced to one, and trailing space on lines is trimmed.
// Unless the result is empty, it is newline-terminated.
func (g *CommentGroup) Text() string {
	if g == nil {
		return ""
	}
	comments := make([]string, len(g.List))
	for i, c := range g.List {
		comments[i] = c.Text
	}

	lines := make([]string, 0, 10) // most comments are less than 10 lines
	for _, c := range comments {
		// Remove comment markers.
		// The parser has given us exactly the comment text.
		switch c[1] {
		case '/':
			//-style comment (no newline at the end)
			c = c[2:]
			if len(c) == 0 {
				// empty line
				break
			}
			if c[0] == ' ' {
				// strip first space - required for Example tests
				c = c[1:]
				break
			}
		case '*':
			/*-style comment */
			c = c[2 : len(c)-2]
		}

		// Split on newlines.
		cl := strings.Split(c, "\n")

		// Walk lines, stripping trailing white space and adding to list.
		for _, l := range cl {
			lines = append(lines, stripTrailingWhitespace(l))
		}
	}

	// Remove leading blank lines; convert runs of
	// interior blank lines to a single blank line.
	n := 0
	for _, line := range lines {
		if line != "" || n > 0 && lines[n-1] != "" {
			lines[n] = line
			n++
		}
	}
	lines = lines[0:n]

	// Add final "" entry to get trailing newline from Join.
	if n > 0 && lines[n-1] != "" {
		lines = append(lines, "")
	}

	return strings.Join(lines, "\n")
}

type Module struct {
	noType
	File  *token.File
	Id    *Identifier
	Decls []Decl

	Imports []*ImportDecl
}

func (p *Module) isNode() {}
func (p *Module) Pos() token.Pos {
	return p.File.Pos(0)
}
func (p *Module) End() token.Pos {
	return p.File.Pos(p.File.Size)
}

// Lookup returns the declaration in the module with the given name or nil if none exists. If
// there are multiple declarations, all declarations are returned (e.g. function overloads).
func (p *Module) Lookup(name string) (result []Decl) {
	for _, d := range p.Decls {
		if d.ExportName() == name {
			result = append(result, d)
		}
	}
	return result
}

type Decl interface {
	Node
	isDeclaration()

	// ExportName is the identifier this declaration is referencable through module name. Returns empty
	// if the declaration is not exported.
	ExportName() string

	// Type returns the details of the declaration (see individual declarations for specifics).
	Type() Type
}

type ImportDecl struct {
	typeNode                // describes imported module's types
	Import   token.Pos      // `import` keyword
	Alias    *Identifier    // name to import (default to last element of path). Can be nil.
	Path     *StringLiteral // value of import
}

func (i *ImportDecl) isDeclaration() {}
func (i *ImportDecl) isNode()        {}
func (i *ImportDecl) Pos() token.Pos {
	return i.Import
}
func (i *ImportDecl) End() token.Pos {
	return i.Path.End()
}
func (i *ImportDecl) ExportName() string { return "" } // imports never exported

// ModuleName extracts the module name from the import declaration. Some examples:
// - otp/random -> random
// - github.com/masp/reiter/reitr_server -> reiter_server
func (i *ImportDecl) ModuleName() string {
	return path.Base(i.Path.Value)
}

// TypeDecl defines a new type, and looks like `[export] type <name> <definition>`
type TypeDecl struct {
	typeNode           // type of the definition
	TypePos  token.Pos // `type` keyword

	Name       *Identifier // the new type name
	Definition Expression  // the type value
}

func (t *TypeDecl) isDeclaration() {}
func (t *TypeDecl) isNode()        {}
func (t *TypeDecl) Pos() token.Pos {
	return t.TypePos
}
func (t *TypeDecl) End() token.Pos {
	return t.Definition.End()
}
func (t *TypeDecl) ExportName() string {
	if t.Name.IsPublic() {
		return t.Name.Name
	}
	return ""
}

type ConstDecl struct {
	typeNode               // type of the constant
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
func (c *ConstDecl) ExportName() string {
	if c.Identifier.IsPublic() {
		return c.Identifier.Name
	}
	return ""
}

type FuncDecl struct {
	typeNode // signature of the function (see subnodes for individual types)

	Func       token.Pos // `func` keyword
	LeftBrace  token.Pos // `{` and `}` token
	RightBrace token.Pos

	Name       *Identifier // function name
	Parameters *FieldList  // function parameters
	Statements []Statement
	ReturnType Expression
}

func (f *FuncDecl) IsPublic() bool {
	return f.Name.IsPublic()
}

func (f *FuncDecl) isDeclaration() {}
func (f *FuncDecl) isExpression()  {} // is an expression without a name
func (f *FuncDecl) isNode()        {}
func (f *FuncDecl) Pos() token.Pos {
	return f.Func
}
func (f *FuncDecl) End() token.Pos {
	return f.RightBrace + 1
}
func (f *FuncDecl) ExportName() string {
	if f.Name.IsPublic() {
		return f.Name.Name
	}
	return ""
}

type BadDecl struct {
	noType
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
func (b *BadDecl) ExportName() string { return "" }
func (b *BadDecl) Type() Type         { return nil }

type Statement interface {
	Node
	isStatement()
}

type BadStmt struct {
	noType
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
func (e *ReturnStatement) Type() Type { return e.Expression.Type() }

type Expression interface {
	Node
	isExpression()
	Type() Type
}

type BadExpr struct {
	noType

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
	Typ   Expression    // field/method/parameter type; or nil
}

func (f *Field) Pos() token.Pos {
	if len(f.Names) > 0 {
		return f.Names[0].Pos()
	}
	if f.Typ != nil {
		return f.Typ.Pos()
	}
	return token.NoPos
}

func (f *Field) End() token.Pos {
	if f.Typ != nil {
		return f.Typ.End()
	}
	if len(f.Names) > 0 {
		return f.Names[len(f.Names)-1].End()
	}
	return token.NoPos
}
func (f *Field) isNode()       {}
func (f *Field) isExpression() {}
func (f *Field) Type() Type    { return f.Typ.Type() }

// A FieldList represents a list of Fields, enclosed by parentheses,
// curly braces, or square brackets.
type FieldList struct {
	noType
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
	typeNode // the type expression for this tuple

	Opener token.Pos    // '('
	Elts   []Expression // types for elements
	Closer token.Pos    // ')'
}

func (t *TupleType) isExpression() {}
func (t *TupleType) isNode()       {}
func (t *TupleType) Pos() token.Pos {
	return t.Opener
}
func (t *TupleType) End() token.Pos {
	return t.Closer + 1
}

// ListType has the form []T
type ListType struct {
	typeNode // the type expression for this list

	Lbrack token.Pos  // position of "["
	Elt    Expression // type expression for the element
	Rbrack token.Pos  // position of "]"
}

func (l *ListType) isExpression() {}
func (l *ListType) isNode()       {}
func (l *ListType) Pos() token.Pos {
	return l.Lbrack
}
func (l *ListType) End() token.Pos {
	return l.Elt.End()
}

// EnumType has the form enum { type... }
type EnumType struct {
	typeNode // the declared type of the enum

	EnumPos token.Pos    // position of "enum" keyword
	Opener  token.Pos    // position of "{"
	Cases   []Expression // type expressions for each enum case (one per line)
	Closer  token.Pos    // position of "}"
}

func (l *EnumType) isExpression() {}
func (l *EnumType) isNode()       {}
func (l *EnumType) Pos() token.Pos {
	return l.Opener
}
func (l *EnumType) End() token.Pos {
	return l.Closer + 1
}

type CallExpr struct {
	typeNode // the type of the return value of Fun

	Fun  Expression
	Args []Expression

	LeftParen, RightParen token.Pos
}

func (u *CallExpr) isExpression()  {}
func (u *CallExpr) isNode()        {}
func (u *CallExpr) Pos() token.Pos { return u.Fun.Pos() }
func (u *CallExpr) End() token.Pos { return u.RightParen + 1 }

type DotExpr struct {
	typeNode // the type of the attribute under X

	X    Expression
	Dot  token.Pos
	Attr *Identifier
}

func (u *DotExpr) isExpression() {}
func (u *DotExpr) isNode()       {}
func (u *DotExpr) Pos() token.Pos {
	return u.X.Pos()
}
func (u *DotExpr) End() token.Pos {
	return u.Attr.End()
}

type UnaryExpr struct {
	typeNode // the type of the result of Op on Right.

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
	typeNode // the type of the result of Left Op Right.

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
	Literal() string
}

type StringLiteral struct {
	typeNode // type of literal

	QuotePos   token.Pos
	Value, Lit string
}

func NewString(tok lexer.Token) *StringLiteral {
	if tok.Lit == "" {
		return &StringLiteral{}
	}
	return &StringLiteral{
		QuotePos: tok.Pos,
		Value:    tok.Lit[1 : len(tok.Lit)-1], // remove quotes
		Lit:      tok.Lit,
	}
}

func (s *StringLiteral) isExpression()   {}
func (s *StringLiteral) isNode()         {}
func (s *StringLiteral) Literal() string { return s.Lit }
func (s *StringLiteral) Pos() token.Pos {
	return s.QuotePos
}
func (s *StringLiteral) End() token.Pos {
	return s.QuotePos + token.Pos(len(s.Value)) + 2 // +2 for quotes
}

type AtomLiteral struct {
	typeNode // type of literal

	QuotePos token.Pos
	Value    string
}

func NewAtom(tok lexer.Token) *AtomLiteral {
	if tok.Lit == "" {
		return &AtomLiteral{}
	}
	return &AtomLiteral{
		QuotePos: tok.Pos,
		Value:    tok.Lit[1 : len(tok.Lit)-1], // remove quotes
	}
}

func (s *AtomLiteral) isExpression()   {}
func (s *AtomLiteral) Literal() string { return "'" + s.Value + "'" }
func (s *AtomLiteral) isNode()         {}
func (s *AtomLiteral) Pos() token.Pos {
	return s.QuotePos
}
func (s *AtomLiteral) End() token.Pos {
	return s.QuotePos + token.Pos(len(s.Value)) + 2 // +2 for quotes
}

type IntLiteral struct {
	typeNode // type of literal

	IntPos token.Pos // position of the first digit
	Lit    string    // raw string, e.g. "12"
	Value  int64     // parsed value
}

func (n *IntLiteral) isExpression() {}
func (s *IntLiteral) isNode()       {}
func (s *IntLiteral) Literal() string {
	return s.Lit
}
func (s *IntLiteral) Pos() token.Pos {
	return s.IntPos
}
func (s *IntLiteral) End() token.Pos {
	return s.IntPos + token.Pos(len(s.Lit))
}

type FloatLiteral struct {
	typeNode // type of literal

	FloatPos token.Pos // position of the first digit
	Lit      string    // raw string, e.g. "12.3"
	Value    float64   // parsed value
}

func (s *FloatLiteral) isExpression() {}
func (s *FloatLiteral) isNode()       {}
func (s *FloatLiteral) Literal() string {
	return s.Lit
}
func (s *FloatLiteral) Pos() token.Pos {
	return s.FloatPos
}
func (s *FloatLiteral) End() token.Pos {
	return s.FloatPos + token.Pos(len(s.Lit))
}

type ListLiteral struct {
	typeNode // type of list (computed from elements)

	Opener token.Pos
	Elts   []Expression
	Closer token.Pos
}

func (l *ListLiteral) isExpression() {}
func (l *ListLiteral) isNode()       {}
func (l *ListLiteral) Literal() string {
	return "[]"
}
func (l *ListLiteral) Pos() token.Pos {
	return l.Opener
}
func (l *ListLiteral) End() token.Pos {
	return l.Closer + 1
}

type TupleLit struct {
	typeNode
	Opener, Closer token.Pos // '(' and ')' if present

	Elts []Expression
}

func (l *TupleLit) isExpression() {}
func (l *TupleLit) isNode()       {}
func (l *TupleLit) Literal() string {
	return "tuple()"
}
func (l *TupleLit) Pos() token.Pos {
	return l.Opener
}
func (l *TupleLit) End() token.Pos {
	return l.Closer + 1
}

type KVExpr struct {
	noType

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
	typeNode // what type the identifier resolves to
	NamePos  token.Pos
	Name     string
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
func (p *ParenExpr) Type() Type { return p.Expression.Type() }

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
func (a *AssignExpr) Type() Type {
	return a.Right.Type()
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
func (a *MatchAssignExpr) Type() Type {
	return a.Right.Type()
}

type Match struct {
	typeNode

	MatchPos       token.Pos  // position of match keyword
	Opener, Closer token.Pos  // '{' and '}' positions
	Value          Expression // value being matched
	Cases          []*Case
}

func (m *Match) isExpression() {}
func (m *Match) isNode()       {}
func (m *Match) Pos() token.Pos {
	return m.MatchPos
}
func (m *Match) End() token.Pos {
	return m.Closer + 1
}

type Case struct {
	CaseKeyword, Colon token.Pos // position of case keyword and preceding colon

	Pattern Expression  // left: expression that is matched against
	Body    []Statement // right: statements that are executed if left matches
}

func (c *Case) isNode() {}
func (c *Case) Pos() token.Pos {
	return c.CaseKeyword
}
func (c *Case) End() token.Pos {
	return c.Body[len(c.Body)-1].End()
}
