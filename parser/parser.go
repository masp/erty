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
		token.EOF:         true,
		token.Func:        true,
		token.TypeKeyword: true,
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

	// Each one of these can signify that the value is a type
	typeStart = map[token.Type]bool{
		token.Identifier:     true,
		token.LParen:         true, // tuple
		token.LSquareBracket: true, // list
		token.Atom:           true,
		token.Func:           true,
		token.Enum:           true,
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

func (p *Parser) peekN(n int) (toks []lexer.Token) {
	for i := p.pos; i < len(p.tokens); i++ {
		tok := p.tokens[i]
		if tok.Type == token.Comment {
			continue
		}
		toks = append(toks, tok)
		if len(toks) == n {
			return toks
		}
	}
	if len(toks) < n {
		for i := 0; i < n-len(toks); i++ {
			toks = append(toks, lexer.Token{Type: token.EOF})
		}
	}
	return toks
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

// finalElt either consumes sep for an end of current node and returns false that there is more statments.
// Or, if closer or EOF is found, the function does nothing and returns true.
func (p *Parser) finalElt(sep, closer token.Type) bool {
	if p.matches(closer, token.EOF) {
		return true
	}

	if p.matches(sep) {
		p.eat()
		// if the final stmt has a semicolon and then the closer
		// e.g. { return a; }
		return p.matches(closer, token.EOF)
	}
	next := p.peek()
	p.error(next.Pos, fmt.Errorf("expected '%v', got %v", sep, next.Type))
	p.advance(stmtStart)
	return false
}

// parseSequence parses a generic sequence of nodes with a given separator. For example,
// a list, a tuple, a enum all are sequences of nodes with different openers and separators.
//
// Should be called after eating the opener to the sequence, and will end with the next token
// being either EOF or the closer.
func parseSequence[T ast.Node](p *Parser, sep, closer token.Type, parseElt func(p *Parser) T) (result []T) {
	for {
		result = append(result, parseElt(p))
		if p.finalElt(sep, closer) {
			return result
		}
	}
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
	if tok := p.eatOnly(token.Identifier, "expected 'module' keyword at start of file"); tok.Type != token.Identifier || tok.Lit != "module" {
		p.advance(declStart)
		if tok.Lit != "module" {
			p.error(tok.Pos, fmt.Errorf("expected 'module' keyword at start of file"))
		}
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

func (p *Parser) parseFunctionHeader() ast.Decl {
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

	return &ast.FuncDecl{
		Name:       ast.NewIdent(name),
		Func:       funcTok.Pos,
		Parameters: params,
		ReturnType: retType,
	}
}
func (p *Parser) parseFunctionBody(fn *ast.FuncDecl) {
	lbrace := p.eatOnly(token.LCurlyBracket, "expected '{' after function parameters")
	body := p.parseBody()
	rbrace := p.eatOnly(token.RCurlyBracket, "expected '}' to end function body")
	fn.Statements = body
	fn.LeftBrace = lbrace.Pos
	fn.RightBrace = rbrace.Pos
}

func (p *Parser) parseParams() *ast.FieldList {
	lparen := p.eatOnly(token.LParen, "expected '(' after function name")
	params := &ast.FieldList{Opening: lparen.Pos}
	i := 0
	var named bool // true if parameters have names
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
		var typ ast.Expression
		var isNamed bool
	parse_name:
		// A set of parameters can take the following form:
		// 1: func(a int)             -> 1 int
		// 2: func(a, b int)          -> 2 ints
		// 3: func(int | string, int) -> 2 ints
		toks := p.peekN(2)
		first, second := toks[0], toks[1]
		if first.Type == token.Identifier && second.Type == token.Comma {
			// case 2: func(a, b int)
			isNamed = true
			name := p.eatOnly(token.Identifier, "expected parameter name")
			names = append(names, ast.NewIdent(name))
			p.eat()         // comma
			goto parse_name // parse rest of names and type
		} else if _, secondIsType := typeStart[second.Type]; first.Type == token.Identifier && secondIsType {
			// case 1: func(a int)
			isNamed = true
			name := p.eatOnly(token.Identifier, "expected parameter name")
			names = append(names, ast.NewIdent(name))
			typ = p.parseType()
		} else if _, firstIsType := typeStart[first.Type]; firstIsType {
			// case 3: unnamed parameter func(int | string, int)
			isNamed = false
			typ = p.parseType()
		}

		if i == 0 {
			named = isNamed
		} else if named != isNamed {
			p.error(first.Pos, fmt.Errorf("mix of named and unnamed parameters"))
		}
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
		return p.parseInt(tok)
	case token.Float:
		return p.parseFloat(tok)
	case token.Identifier:
		return ast.NewIdent(tok)
	case token.String:
		return ast.NewString(tok)
	case token.Atom:
		return ast.NewAtom(tok)
	case token.Match:
		return p.parseMatch(tok)
	case token.LParen:
		if p.matches(token.RParen) {
			closer := p.eat()
			return &ast.TupleLit{Opener: tok.Pos, Closer: closer.Pos}
		}

		first := p.parseExpression()
		if p.matches(token.Comma) {
			p.eat()
			rest := parseSequence(p, token.Comma, token.RParen, func(p *Parser) ast.Expression {
				return p.parseExpression()
			})
			rparen := p.eatOnly(token.RParen, "expected ')' to close tuple")
			return &ast.TupleLit{
				Opener: tok.Pos,
				Elts:   append([]ast.Expression{first}, rest...),
				Closer: rparen.Pos,
			}
		} else {
			rparen := p.eatOnly(token.RParen, "unclosed '(' around expression")
			return &ast.ParenExpr{
				Expression: first,
				LParen:     tok.Pos,
				RParen:     rparen.Pos,
			}
		}
	case token.LSquareBracket:
		if p.matches(token.RSquareBracket) {
			// Either it's an empty list (value) or a list type
			// 1. Empty list will not have a type annotation after the []
			// 2. List type will have a type annotation after the []
			rbracket := p.eat()

			if _, isType := typeStart[p.peek().Type]; isType {
				eltType := p.parseType()
				return &ast.ListType{Lbrack: tok.Pos, Elt: eltType, Rbrack: rbracket.Pos}
			} else {
				// Empty list
				return &ast.ListLiteral{Opener: tok.Pos, Closer: rbracket.Pos}
			}
		}

		elts := parseSequence(p, token.Comma, token.RSquareBracket, func(p *Parser) ast.Expression {
			return p.parseExpression()
		})
		rbracket := p.eatOnly(token.RSquareBracket, "unclosed '[' around list")
		return &ast.ListLiteral{Opener: tok.Pos, Elts: elts, Closer: rbracket.Pos}
	case token.Func:
		params := p.parseParams()

		var retType ast.Expression
		if _, isType := typeStart[p.peek().Type]; isType {
			// If the following token could be a type, it must be our return type
			retType = p.parseType()
		}
		decl := &ast.FuncDecl{
			Func:       tok.Pos,
			Parameters: params,
			ReturnType: retType,
		}
		p.parseFunctionBody(decl)
		return decl
	default:
		p.error(tok.Pos, fmt.Errorf("expected expression, got %s", tok.Type.String()))
		to := p.advance(exprEnd)
		return &ast.BadExpr{From: tok.Pos, To: to.Pos}
	}
}

// parseInt converts a string to an integer.
func (p *Parser) parseInt(tok lexer.Token) *ast.IntLiteral {
	v, err := strconv.ParseInt(tok.Lit, 10, 64)
	if err != nil {
		p.error(tok.Pos, fmt.Errorf("parse int: %s", err))
	}
	return &ast.IntLiteral{
		IntPos: tok.Pos,
		Lit:    tok.Lit,
		Value:  v,
	}
}

// parseFloat converts a string to a floating point number
func (p *Parser) parseFloat(tok lexer.Token) *ast.FloatLiteral {
	v, err := strconv.ParseFloat(tok.Lit, 64)
	if err != nil {
		p.error(tok.Pos, fmt.Errorf("parse float: %s", err))
	}
	return &ast.FloatLiteral{
		FloatPos: tok.Pos,
		Lit:      tok.Lit,
		Value:    v,
	}
}
