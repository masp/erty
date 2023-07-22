package parser

import (
	"errors"
	"fmt"
	"strconv"

	"github.com/masp/ertylang/ast"
	"github.com/masp/ertylang/lexer"
	"github.com/masp/ertylang/token"
)

const maxErrors = 10

var (
	ErrBailout   = errors.New("too many errors")
	ErrBadModule = errors.New("module header is not valid")
)

var (
	declStart = map[token.Type]bool{
		token.EOF:  true,
		token.Func: true,
	}

	curlyEnd = map[token.Type]bool{
		token.RCurlyBracket: true,
		token.EOF:           true,
	}

	exprEnd = map[token.Type]bool{
		token.EOF:           true,
		token.Semicolon:     true,
		token.RParen:        true,
		token.RCurlyBracket: true,
		token.Comma:         true,
	}

	stmtStart = map[token.Type]bool{
		token.Return:        true,
		token.Identifier:    true, // assignment
		token.LCurlyBracket: true, // block/tuple
	}

	caseStart = map[token.Type]bool{
		token.Case:          true,
		token.EOF:           true,
		token.RCurlyBracket: true,
	}

	paramStart = map[token.Type]bool{
		token.Identifier: true,
		token.RParen:     true,
	}
)

type Parser struct {
	tokens []lexer.Token
	file   *token.File
	pos    int

	errors token.ErrorList
}

func (p *Parser) advance(to map[token.Type]bool) (tok lexer.Token) {
	for p.peek().Type != token.EOF && !to[p.peek().Type] {
		tok = p.eat()
	}
	return
}

func (p *Parser) eat() (tok lexer.Token) {
	for ; p.pos < len(p.tokens); p.pos++ {
		tok = p.tokens[p.pos]
		if tok.Type == token.Comment {
			continue
		}
		p.pos++
		return tok
	}
	return lexer.Token{Type: token.EOF}
}

func (p *Parser) eatAll(tokenType token.Type) token.Type {
	for {
		if p.pos >= len(p.tokens) {
			return token.EOF
		}
		if p.tokens[p.pos].Type == tokenType {
			p.pos++
		} else {
			return tokenType
		}
	}
}

func (p *Parser) eatOnly(tokenType token.Type, errfmt string, args ...any) lexer.Token {
	tok := p.eat()
	if tok.Type != tokenType {
		errmsg := fmt.Sprintf(errfmt, args...)
		p.error(tok.Pos, fmt.Errorf("%s, got %s", errmsg, tok.String()))
	}
	return tok
}

func (p *Parser) peek() (tok lexer.Token) {
	for i := p.pos; i < len(p.tokens); i++ {
		tok = p.tokens[i]
		if tok.Type == token.Comment {
			continue
		}
		return tok
	}
	return lexer.Token{Type: token.EOF}
}

func (p *Parser) matches(types ...token.Type) bool {
	next := p.peek()
	for _, t := range types {
		if next.Type == t {
			return true
		}
	}
	return false
}

func (p *Parser) error(pos token.Pos, err error) {
	epos := p.file.Position(pos)
	n := len(p.errors)
	if n > 0 && p.errors[n-1].Pos.Line == epos.Line {
		return // discard - likely a spurious error
	}
	if len(p.errors) > maxErrors {
		panic(ErrBailout)
	}
	p.errors.Add(epos, err)
}

func (p *Parser) catchErrors() token.ErrorList {
	if r := recover(); r != nil {
		if r == ErrBailout {
			return p.errors
		} else {
			panic(r)
		}
	}
	return p.errors
}

func (p *Parser) parseModuleHeader(mod *ast.Module, file *token.File) error {
	if tok := p.eatOnly(token.Module, "expected 'module' keyword at start of file"); tok.Type != token.Module {
		p.advance(declStart)
		return ErrBadModule
	}
	name := p.eatOnly(token.Identifier, "expected module name after 'module' keyword")
	if name.Type != token.Identifier {
		p.advance(declStart)
		return ErrBadModule
	}
	mod.Id = ast.NewIdent(name)

	if !p.matches(token.Semicolon, token.EOF) {
		p.eatOnly(token.Semicolon, "expected ';' after module name")
	}
	p.eatAll(token.Semicolon)

	mod.Imports = p.parseImports(mod)
	return nil
}

func (p *Parser) parseImports(mod *ast.Module) []*ast.ImportDecl {
	var imports []*ast.ImportDecl
	for p.matches(token.Import) {
		imp := p.parseImport(mod)
		if imp != nil {
			mod.Decls = append(mod.Decls, imp)
		}

		if imp, ok := imp.(*ast.ImportDecl); ok {
			imports = append(imports, imp)
			if !p.matches(token.Semicolon, token.EOF) {
				p.eatOnly(token.Semicolon, "expected ';' after import declaration")
			}
			p.eatAll(token.Semicolon)
		}
	}
	return imports
}

func (p *Parser) parseImport(mod *ast.Module) ast.Decl {
	importTok := p.eatOnly(token.Import, "expected 'import' keyword at start of import declaration")
	if importTok.Type != token.Import {
		to := p.advance(declStart)
		return &ast.BadDecl{From: importTok.Pos, To: to.Pos}
	}

	var alias *ast.Identifier
	if p.matches(token.Identifier) {
		alias = ast.NewIdent(p.eat())
	}

	path := p.eatOnly(token.String, "expected module path after 'import' keyword")
	if path.Type != token.String {
		to := p.advance(declStart)
		return &ast.BadDecl{From: importTok.Pos, To: to.Pos}
	}

	return &ast.ImportDecl{
		Import: importTok.Pos,
		Alias:  alias,
		Path:   ast.NewString(path),
	}
}

func (p *Parser) parseTypeDecl() ast.Decl {
	typeTok := p.eatOnly(token.TypeKeyword, "expected 'type' keyword at start of type declaration")
	if typeTok.Type != token.TypeKeyword {
		to := p.advance(declStart)
		return &ast.BadDecl{From: typeTok.Pos, To: to.Pos}
	}

	name := p.eatOnly(token.Identifier, "expected type name after 'type' keyword")
	if name.Type != token.Identifier {
		to := p.advance(declStart)
		return &ast.BadDecl{From: typeTok.Pos, To: to.Pos}
	}

	def := p.parseType()
	return &ast.TypeDecl{
		TypePos:    typeTok.Pos,
		Name:       ast.NewIdent(name),
		Definition: def,
	}
}

func (p *Parser) parseFunction() ast.Decl {
	funcTok := p.eatOnly(token.Func, "expected 'func' keyword at start of function")
	if funcTok.Type != token.Func {
		to := p.advance(declStart)
		return &ast.BadDecl{From: funcTok.Pos, To: to.Pos}
	}

	name := p.eatOnly(token.Identifier, "expected function name after 'func' keyword")
	if name.Type != token.Identifier {
		to := p.advance(declStart)
		return &ast.BadDecl{From: funcTok.Pos, To: to.Pos}
	}
	params := p.parseParams()

	var retType ast.Expression
	if !p.matches(token.EOF, token.LCurlyBracket) {
		retType = p.parseExpression()
	}

	lbrace := p.eatOnly(token.LCurlyBracket, "expected '{' after function parameters")
	body := p.parseBody()
	rbrace := p.eatOnly(token.RCurlyBracket, "expected '}' to end function body")
	return &ast.FuncDecl{
		Name:       ast.NewIdent(name),
		Func:       funcTok.Pos,
		Statements: body,
		Parameters: params,
		ReturnType: retType,
		LeftBrace:  lbrace.Pos,
		RightBrace: rbrace.Pos,
	}
}

func (p *Parser) parseParams() *ast.FieldList {
	lparen := p.eatOnly(token.LParen, "expected '(' after function name")
	params := &ast.FieldList{Opening: lparen.Pos}
	i := 0
	for !p.matches(token.EOF) {
		if p.matches(token.RParen) {
			rparen := p.eat()
			params.Closing = rparen.Pos
			break
		}
		if i > 0 {
			if tok := p.eatOnly(token.Comma, "expected ',' between parameters"); tok.Type != token.Comma {
				p.advance(paramStart)
			}
		}
		var names []*ast.Identifier
	parse_name:
		name := p.eatOnly(token.Identifier, "expected parameter name")
		if name.Type != token.Identifier {
			break
		}
		names = append(names, ast.NewIdent(name))
		if p.matches(token.Comma) {
			// More than 1 name with same type (e.g. a, b, c int)
			p.eat()
			goto parse_name
		}
		typ := p.parseType()
		params.List = append(params.List, &ast.Field{
			Names: names,
			Typ:   typ,
		})
		i++
	}
	return params
}

func (p *Parser) parseBody() []ast.Statement {
	var body []ast.Statement
	for !p.matches(token.EOF) {
		p.eatAll(token.Semicolon) // eat all empty statements
		tok := p.peek()
		if tok.Type == token.RCurlyBracket {
			break
		}

		statement := p.parseStatement(tok)
		if statement != nil {
			body = append(body, statement)
		}
		if !p.matches(token.Semicolon, token.RCurlyBracket, token.EOF) {
			from := p.eat()
			p.error(from.Pos, fmt.Errorf("expected ';' at end of statement"))
			p.advance(exprEnd) // make sure we clear the line before we try to find a new statement
			to := p.advance(stmtStart)
			body = append(body, &ast.BadStmt{From: from.Pos, To: to.Pos})
		}
	}
	return body
}

func (p *Parser) parseStatement(tok lexer.Token) ast.Statement {
	switch tok.Type {
	case token.Return:
		return p.parseReturnStatement()
	default: // expression statement
		return p.parseExpressionStatement(tok)
	}
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	retTok := p.eatOnly(token.Return, "expected 'return' keyword")
	return &ast.ReturnStatement{
		Return:     retTok.Pos,
		Expression: p.parseExpression(),
	}
}

func (p *Parser) parseExpressionStatement(tok lexer.Token) *ast.ExprStatement {
	return &ast.ExprStatement{Expression: p.parseExpression()}
}

// The order of precedence is defined by which parse* function is called first.
// The BNF for the parsing looks like:
// expression     → match ;
// match          → equality ( ( "=" | ":=" ) equality ) ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" | "+" ) unary
//                | primary ;
// call		      → primary ( "(" arguments? ")" | "." IDENTIFIER)* ;
// arguments      → expression ( "," expression )* ;
// primary        → NUMBER | STRING | ATOM | IDENTIFIER
//                | "(" expression ")" ;

func (p *Parser) parseExpression() ast.Expression {
	return p.parseAssign()
}

func (p *Parser) parseAssign() ast.Expression {
	left := p.parseEquality()
	// just if and not while because these are right-associative
	if p.matches(token.Equal) {
		equals := p.eat()
		right := p.parseAssign()
		if leftId, ok := left.(*ast.Identifier); ok {
			return &ast.AssignExpr{
				Left:   leftId,
				Equals: equals.Pos,
				Right:  right,
			}
		} else {
			pos := equals.Pos
			if left != nil {
				pos = left.Pos()
			}
			p.error(pos, fmt.Errorf("left hand side of assignment must be an identifier"))
			return nil
		}
	} else if p.matches(token.ColonEqual) {
		equals := p.eat()
		right := p.parseEquality()
		left = &ast.MatchAssignExpr{
			Left:   left,
			Equals: equals.Pos,
			Right:  right,
		}
	}
	return left
}

func (p *Parser) parseEquality() ast.Expression {
	left := p.parseComparison()
	for p.matches(token.EqualEqual, token.BangEqual) {
		op := p.eat()
		right := p.parseComparison()
		left = &ast.BinaryExpr{
			Left:  left,
			Op:    op.Type,
			OpPos: op.Pos,
			Right: right,
		}
	}
	return left
}

func (p *Parser) parseComparison() ast.Expression {
	left := p.parseTerm()
	for p.matches(token.Greater, token.GreaterEqual, token.Less, token.LessEqual) {
		op := p.eat()
		right := p.parseTerm()
		left = &ast.BinaryExpr{
			Left:  left,
			Op:    op.Type,
			OpPos: op.Pos,
			Right: right,
		}
	}
	return left
}

func (p *Parser) parseTerm() ast.Expression {
	left := p.parseFactor()
	for p.matches(token.Plus, token.Minus) {
		op := p.eat()
		right := p.parseFactor()
		left = &ast.BinaryExpr{
			Left:  left,
			Op:    op.Type,
			OpPos: op.Pos,
			Right: right,
		}
	}
	return left
}

func (p *Parser) parseFactor() ast.Expression {
	left := p.parseUnary()
	for p.matches(token.Slash, token.Star) {
		op := p.eat()
		right := p.parseUnary()
		left = &ast.BinaryExpr{
			Left:  left,
			Op:    op.Type,
			OpPos: op.Pos,
			Right: right,
		}
	}
	return left
}

func (p *Parser) parseUnary() ast.Expression {
	if p.matches(token.Minus, token.Plus, token.Bang) {
		op := p.eat()
		return &ast.UnaryExpr{
			Op:    op.Type,
			OpPos: op.Pos,
			Right: p.parseUnary(),
		}
	}
	return p.parseCall()
}

func (p *Parser) parseCall() ast.Expression {
	callee := p.parsePrimary()
	for {
		if p.matches(token.LParen) {
			lparen := p.eat()
			args := p.parseArguments()
			rparen := p.eat()
			callee = &ast.CallExpr{
				Fun:        callee,
				Args:       args,
				LeftParen:  lparen.Pos,
				RightParen: rparen.Pos,
			}
		} else if p.matches(token.Period) {
			dot := p.eat()
			name := p.eatOnly(token.Identifier, "expected identifier after '.'")
			if name.Type != token.Identifier {
				p.advance(exprEnd)
				return &ast.BadExpr{From: name.Pos, To: name.Pos}
			}
			callee = &ast.DotExpr{
				Dot:  dot.Pos,
				X:    callee,
				Attr: ast.NewIdent(name),
			}
		} else {
			break
		}
	}
	return callee
}

func (p *Parser) parseArguments() []ast.Expression {
	var args []ast.Expression
	if !p.matches(token.RParen) {
		args = append(args, p.parseExpression())
		for p.matches(token.Comma) {
			comma := p.eat()
			if len(args) >= 255 {
				p.error(comma.Pos, fmt.Errorf("cannot have more than 255 arguments"))
				return args
			}
			args = append(args, p.parseExpression())
		}
	}
	return args
}

func (p *Parser) parsePrimary() ast.Expression {
	tok := p.eat()
	switch tok.Type {
	case token.Integer:
		return &ast.IntLiteral{
			IntPos: tok.Pos,
			Lit:    tok.Lit,
			Value:  p.parseInt(tok),
		}
	case token.Float:
		return &ast.FloatLiteral{
			FloatPos: tok.Pos,
			Lit:      tok.Lit,
			Value:    p.parseFloat(tok),
		}
	case token.Identifier:
		id := &ast.Identifier{NamePos: tok.Pos, Name: tok.Lit}
		if p.matches(token.Identifier, token.Tuple, token.LSquareBracket) {
			// If it looks like a type, wrap the id in an ast.Field
			typ := p.parseType()
			return &ast.Field{Names: []*ast.Identifier{id}, Typ: typ}
		}
		return id
	case token.String:
		return ast.NewString(tok)
	case token.Atom:
		return ast.NewAtom(tok)
	case token.Match:
		return p.parseMatch(tok)
	case token.LParen:
		expr := p.parseExpression()
		rparen := p.eatOnly(token.RParen, "unclosed '(' around expression")
		return &ast.ParenExpr{
			Expression: expr,
			LParen:     tok.Pos,
			RParen:     rparen.Pos,
		}
	case token.LSquareBracket:
		if p.matches(token.RSquareBracket) {
			// Either it's an empty list (value) or a list type
			// 1. Empty list will not have a type annotation after the []
			// 2. List type will have a type annotation after the []
			rbracket := p.eat()

			if p.matches(token.Identifier, token.Tuple, token.Map, token.LSquareBracket) {
				eltType := p.parseType()
				return &ast.ListType{Lbrack: tok.Pos, Elt: eltType, Rbrack: rbracket.Pos}
			} else {
				// Empty list
				return &ast.ListLiteral{Opener: tok.Pos, Closer: rbracket.Pos}
			}
		}

		elts := p.parseList(token.RSquareBracket)
		rbracket := p.eatOnly(token.RSquareBracket, "unclosed '[' around list")
		return &ast.ListLiteral{Opener: tok.Pos, Elts: elts, Closer: rbracket.Pos}
	default:
		p.error(tok.Pos, fmt.Errorf("expected expression, got %s", tok.Type.String()))
		to := p.advance(exprEnd)
		return &ast.BadExpr{From: tok.Pos, To: to.Pos}
	}
}

func (p *Parser) parseList(end token.Type) []ast.Expression {
	var elements []ast.Expression
	for !p.matches(end) {
		elements = append(elements, p.parseExpression())
		next := p.peek()
		switch next.Type {
		case end:
			break
		case token.Comma:
			p.eat()
		default:
			p.error(next.Pos, fmt.Errorf("expected ',' or '%s', got %s", end.String(), next.Type))
			return elements
		}
	}
	return elements
}

// parseInt converts a string to an integer.
func (p *Parser) parseInt(tok lexer.Token) int64 {
	v, err := strconv.ParseInt(tok.Lit, 10, 64)
	if err != nil {
		p.error(tok.Pos, fmt.Errorf("parse int: %s", err))
	}
	return v
}

// parseFloat converts a string to a floating point number
func (p *Parser) parseFloat(tok lexer.Token) float64 {
	v, err := strconv.ParseFloat(tok.Lit, 64)
	if err != nil {
		p.error(tok.Pos, fmt.Errorf("parse float: %s", err))
	}
	return v
}

// parseType only parses types expressions. Types can either be identifiers,
// or one of the built in types like lists, tuples, binaries, etc...
func (p *Parser) parseType() ast.Expression {
	tok := p.eat()
	switch tok.Type {
	case token.Identifier: // external type, built-in type (like string)
		ident := ast.NewIdent(tok)
		if p.matches(token.Period) {
			// dot expr
			dot := p.eat()
			attr := p.eatOnly(token.Identifier, "expected identifier after '.'")
			if attr.Type != token.Identifier {
				return &ast.BadExpr{From: dot.Pos, To: attr.Pos}
			}
			return &ast.DotExpr{X: ident, Dot: dot.Pos, Attr: ast.NewIdent(attr)}
		}
		return ident
	case token.Tuple: // tuple[...]
		return p.parseTupleType(tok)
	case token.LSquareBracket: // []type
		rbrack := p.eatOnly(token.RSquareBracket, "expected ']' after '[' for list type")
		if rbrack.Type != token.RSquareBracket {
			return &ast.BadExpr{From: tok.Pos, To: rbrack.Pos}
		}
		eltType := p.parseType()
		return &ast.ListType{Lbrack: tok.Pos, Elt: eltType, Rbrack: rbrack.Pos}
	default:
		p.error(tok.Pos, fmt.Errorf("expected type, got %s", tok.Type.String()))
		return &ast.BadExpr{From: tok.Pos, To: tok.Pos}
	}
}

// parseTupleType parses a tuple of the form `tuple[<fieldlist>]` and returns
// the resulting expression. A tuple can look like:
// - tuple[] (only empty tuple {} allowed)
// - tuple[int, int] (2 ints)
func (p *Parser) parseTupleType(tupleTok lexer.Token) *ast.TupleType {
	lbracket := p.eatOnly(token.LSquareBracket, "expected '[' after 'tuple'")
	fields := &ast.FieldList{}
	for !p.matches(token.RSquareBracket) {
		typExpr := p.parseType()
		fields.List = append(fields.List, &ast.Field{Typ: typExpr})
		if p.matches(token.RSquareBracket) {
			break
		}
		p.eatOnly(token.Comma, "missing ',' in tuple type list")
	}

	fields.Opening = lbracket.Pos
	rbracket := p.eatOnly(token.RSquareBracket, "expected ']' after tuple field list")
	fields.Closing = rbracket.Pos
	return &ast.TupleType{
		Tuple: tupleTok.Pos,
		Elts:  fields,
	}
}
