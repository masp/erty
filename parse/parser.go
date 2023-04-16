package parse

import (
	"errors"
	"fmt"
	"strconv"

	"github.com/masp/garlang/ast"
	"github.com/masp/garlang/lexer"
	"github.com/masp/garlang/token"
)

const maxErrors = 10

var (
	ErrTooManyErrors = errors.New("too many errors")
)

var (
	declStart = map[token.Type]bool{
		token.EOF:  true,
		token.Func: true,
	}

	exprEnd = map[token.Type]bool{
		token.EOF:        true,
		token.Semicolon:  true,
		token.RightParen: true,
		token.RightBrace: true,
		token.Comma:      true,
	}

	stmtStart = map[token.Type]bool{
		token.Return: true,
	}
)

type Parser struct {
	tokens []lexer.Token
	pos    int

	errors []error
}

func (p *Parser) advance(to map[token.Type]bool) {
	for p.peek().Type != token.EOF && !to[p.peek().Type] {
		p.eat()
	}
}

func (p *Parser) eat() lexer.Token {
	if p.pos >= len(p.tokens) {
		p.error(fmt.Errorf("unexpected end of file"))
		return lexer.Token{Type: token.EOF}
	}
	token := p.tokens[p.pos]
	p.pos++
	return token
}

func (p *Parser) eatAll(tokenType token.Type) {
	if p.pos >= len(p.tokens) {
		return
	}
	for p.tokens[p.pos].Type == tokenType {
		p.pos++
	}
}

func (p *Parser) eatOnly(tokenType token.Type, errfmt string, args ...any) lexer.Token {
	tok := p.eat()
	if tok.Type == token.EOF {
		return tok
	}
	if tok.Type != tokenType {
		errmsg := fmt.Sprintf(errfmt, args...)
		p.error(fmt.Errorf("%s, got %s", errmsg, tok.Value))
	}
	return tok
}

func (p *Parser) peek() lexer.Token {
	if p.pos >= len(p.tokens) {
		return lexer.Token{Type: token.EOF}
	}
	return p.tokens[p.pos]
}

func (p *Parser) matches(types ...token.Type) bool {
	for _, t := range types {
		if p.peek().Type == t {
			return true
		}
	}
	return false
}

func (p *Parser) error(err error) {
	if len(p.errors) > maxErrors {
		panic(ErrTooManyErrors)
	}
	p.errors = append(p.errors, err)
}

func Module(tokens []lexer.Token) (mod *ast.Module, err error) {
	parser := &Parser{
		tokens: tokens,
	}

	mod = parser.parseModuleHeader()

	defer parser.catchErrors(&err)

	for {
		tok := parser.peek()
		if tok.Type == token.EOF {
			break
		}
		tok = parser.eat()

		switch tok.Type {
		case token.Func:
			mod.Functions = append(mod.Functions, parser.parseFunction())
		case token.Export:
			if tok := parser.eatOnly(token.Func, "expected 'func' after 'export' keyword"); tok.Type != token.Func {
				parser.advance(declStart)
				continue
			}
			mod.Functions = append(mod.Functions, parser.parseFunction())
			mod.Functions[len(mod.Functions)-1].Exported = true
		case token.Semicolon:
			continue
		default:
			parser.error(fmt.Errorf("expected func, got %q (%s)", tok.Value, tok.Type.String()))
			parser.advance(declStart)
		}
	}
	return mod, errors.Join(parser.errors...)
}

func (p *Parser) catchErrors(out *error) {
	if r := recover(); r != nil {
		if r == ErrTooManyErrors {
			*out = errors.Join(p.errors...)
		} else {
			panic(r)
		}
	}
}

func (p *Parser) parseModuleHeader() *ast.Module {
	if tok := p.eatOnly(token.Module, "epxected 'module' keyword at start of file"); tok.Type != token.Module {
		p.advance(declStart)
		return &ast.Module{}
	}
	name := p.eatOnly(token.Identifier, "expected module name after 'module' keyword")
	if name.Type != token.Identifier {
		p.advance(declStart)
		return &ast.Module{}
	}
	return &ast.Module{
		Name: name.Value,
	}
}

func Function(tokens []lexer.Token) (function ast.FuncDecl, err error) {
	parser := &Parser{
		tokens: tokens,
	}

	defer parser.catchErrors(&err)

	parser.eatOnly(token.Func, "expected 'func' keyword at start of function")
	return parser.parseFunction(), errors.Join(parser.errors...)
}

func (p *Parser) parseFunction() ast.FuncDecl {
	identifier := p.eatOnly(token.Identifier, "expected function name after 'func' keyword")
	p.eatOnly(token.LeftParen, "expected '(' after function name")
	params := p.parseParams()

	p.eatOnly(token.LeftBrace, "expected '{' after function parameters")
	body := p.parseBody()
	return ast.FuncDecl{
		Name:       identifier.Value,
		Statements: body,
		Parameters: params,
	}
}

func (p *Parser) parseParams() []ast.Identifier {
	var params []ast.Identifier
	i := 0
	for {
		if p.matches(token.RightParen) {
			p.eat()
			break
		}
		if i > 0 {
			if tok := p.eatOnly(token.Comma, "expected ',' between parameters"); tok.Type != token.Comma {
				p.advance(exprEnd)
			}
		}
		name := p.eatOnly(token.Identifier, "expected parameter name")
		params = append(params, ast.Identifier{Name: name})
		i++
	}
	return params
}

func (p *Parser) parseBody() []ast.Statement {
	var body []ast.Statement
	for {
		tok := p.peek()
		if tok.Type == token.RightBrace {
			p.eat()
			break
		}
		statement := p.parseStatement(tok)
		if statement != nil {
			body = append(body, statement)
		}
		p.eatAll(token.Semicolon)
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

func (p *Parser) parseReturnStatement() ast.ReturnStatement {
	p.eatOnly(token.Return, "expected 'return' keyword")
	return ast.ReturnStatement{
		Expression: p.parseExpression(),
	}
}

func (p *Parser) parseExpressionStatement(tok lexer.Token) ast.ExprStatement {
	return ast.ExprStatement{Expression: p.parseExpression()}
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
	return p.parseMatch()
}

func (p *Parser) parseMatch() ast.Expression {
	left := p.parseEquality()
	// just if and not while because these are right-associative
	if p.matches(token.Equal) {
		p.eat()
		right := p.parseMatch()
		if left, ok := left.(ast.Identifier); ok {
			return ast.AssignExpr{
				Left:  left.Name,
				Right: right,
			}
		} else {
			p.error(fmt.Errorf("left hand side of assignment must be an identifier"))
			return nil
		}
	} else if p.matches(token.ColonEqual) {
		p.eat()
		right := p.parseEquality()
		left = ast.MatchAssignExpr{
			Left:  left,
			Right: right,
		}
	}
	return left
}

func (p *Parser) parseEquality() ast.Expression {
	left := p.parseComparison()
	for p.matches(token.EqualEqual, token.BangEqual) {
		op := p.eat()
		right := p.parseComparison()
		left = ast.BinaryExpr{
			Left:     left,
			Operator: op.Value,
			Right:    right,
		}
	}
	return left
}

func (p *Parser) parseComparison() ast.Expression {
	left := p.parseTerm()
	for p.matches(token.Greater, token.GreaterEqual, token.Less, token.LessEqual) {
		op := p.eat()
		right := p.parseTerm()
		left = ast.BinaryExpr{
			Left:     left,
			Operator: op.Value,
			Right:    right,
		}
	}
	return left
}

func (p *Parser) parseTerm() ast.Expression {
	left := p.parseFactor()
	for p.matches(token.Plus, token.Minus) {
		op := p.eat()
		right := p.parseFactor()
		left = ast.BinaryExpr{
			Left:     left,
			Operator: op.Value,
			Right:    right,
		}
	}
	return left
}

func (p *Parser) parseFactor() ast.Expression {
	left := p.parseUnary()
	for p.matches(token.Slash, token.Star) {
		op := p.eat()
		right := p.parseUnary()
		left = ast.BinaryExpr{
			Left:     left,
			Operator: op.Value,
			Right:    right,
		}
	}
	return left
}

func (p *Parser) parseUnary() ast.Expression {
	if p.matches(token.Minus, token.Plus) {
		op := p.eat()
		return ast.UnaryExpr{
			Operator: op.Value,
			Right:    p.parseUnary(),
		}
	}
	return p.parseCall()
}

func (p *Parser) parseCall() ast.Expression {
	callee := p.parsePrimary()
	for {
		if p.matches(token.LeftParen) {
			p.eat()
			args := p.parseArguments()
			callee = ast.CallExpr{
				Callee:    callee,
				Arguments: args,
			}
		} else if p.matches(token.Period) {
			p.eat()
			name := p.eatOnly(token.Identifier, "expected identifier after '.'")
			callee = ast.DotExpr{
				Target:    callee,
				Attribute: name,
			}
		} else {
			break
		}
	}
	return callee
}

func (p *Parser) parseArguments() []ast.Expression {
	var args []ast.Expression
	if !p.matches(token.RightParen) {
		args = append(args, p.parseExpression())
		for p.matches(token.Comma) {
			if len(args) >= 255 {
				p.error(fmt.Errorf("cannot have more than 255 arguments"))
				return args
			}
			p.eat()
			args = append(args, p.parseExpression())
		}
	}
	p.eat()
	return args
}

func (p *Parser) parsePrimary() ast.Expression {
	tok := p.eat()
	switch tok.Type {
	case token.Integer:
		return ast.IntLiteral{
			Value: p.parseInt(tok),
		}
	case token.Float:
		return ast.FloatLiteral{
			Value: p.parseFloat(tok),
		}
	case token.Identifier:
		return ast.Identifier{
			Name: tok,
		}
	case token.String:
		return ast.StringLiteral{
			Value: tok.Value,
		}
	case token.Atom:
		return ast.AtomLiteral{
			Value: tok.Value,
		}
	case token.LeftParen:
		expr := p.parseExpression()
		p.eatOnly(token.RightParen, "unclosed '(' around expression")
		return ast.ParenExpr{Expression: expr}
	default:
		p.error(fmt.Errorf("expected expression, got %s", tok.Type.String()))
		return nil
	}
}

// parseInt converts a string to an integer.
func (p *Parser) parseInt(token lexer.Token) int64 {
	v, err := strconv.ParseInt(token.Value, 10, 64)
	if err != nil {
		p.error(fmt.Errorf("parse int: %s", err))
	}
	return v
}

// parseFloat converts a string to a floating point number
func (p *Parser) parseFloat(token lexer.Token) float64 {
	v, err := strconv.ParseFloat(token.Value, 64)
	if err != nil {
		p.error(fmt.Errorf("parse float: %s", err))
	}
	return v
}
