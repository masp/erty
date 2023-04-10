package parse

import (
	"errors"
	"fmt"
	"strconv"

	"github.com/masp/garlang/ast"
	"github.com/masp/garlang/lexer"
)

const maxErrors = 10

var (
	ErrTooManyErrors = errors.New("too many errors")
)

type Parser struct {
	tokens []lexer.Token
	pos    int

	errors []error
}

func (p *Parser) eat() lexer.Token {
	if p.pos >= len(p.tokens) {
		p.error(fmt.Errorf("unexpected end of file"))
		return lexer.Token{Type: lexer.EOF}
	}
	token := p.tokens[p.pos]
	p.pos++
	return token
}

func (p *Parser) eatAll(tokenType lexer.TokenType) {
	if p.pos >= len(p.tokens) {
		return
	}
	for p.tokens[p.pos].Type == tokenType {
		p.pos++
	}
}

func (p *Parser) eatOnly(tokenType lexer.TokenType) lexer.Token {
	token := p.eat()
	if token.Type == lexer.EOF {
		return token
	}
	if token.Type != tokenType {
		p.error(fmt.Errorf("expected %s, got %s", tokenType.String(), token.Type.String()))
	}
	return token
}

func (p *Parser) peek() lexer.Token {
	if p.pos >= len(p.tokens) {
		return lexer.Token{Type: lexer.EOF}
	}
	return p.tokens[p.pos]
}

func (p *Parser) matches(types ...lexer.TokenType) bool {
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
		token := parser.peek()
		if token.Type == lexer.EOF {
			break
		}
		token = parser.eat()

		switch token.Type {
		case lexer.Func:
			mod.Functions = append(mod.Functions, parser.parseFunction())
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
	p.eatOnly(lexer.Module)
	name := p.eatOnly(lexer.String)
	return &ast.Module{
		Name: name.Value,
	}
}

func Function(tokens []lexer.Token) (function ast.FuncDecl, err error) {
	parser := &Parser{
		tokens: tokens,
	}

	defer parser.catchErrors(&err)

	parser.eatOnly(lexer.Func)
	return parser.parseFunction(), errors.Join(parser.errors...)
}

func (p *Parser) parseFunction() ast.FuncDecl {
	identifier := p.eatOnly(lexer.Identifier)
	p.eatOnly(lexer.LeftParen)
	params := p.parseParams()

	p.eatOnly(lexer.LeftBrace)
	body := p.parseBody()
	return ast.FuncDecl{
		Name:       identifier.Value,
		Statements: body,
		Parameters: params,
	}
}

func (p *Parser) parseParams() []ast.Identifier {
	var params []ast.Identifier
	for {
		token := p.eat()
		if token.Type == lexer.RightParen {
			break
		}
		if token.Type != lexer.Identifier {
			p.error(fmt.Errorf("expected identifier, got %s", token.Type.String()))
		}
		params = append(params, ast.Identifier{Name: token})
		p.eatAll(lexer.Comma)
	}
	return params
}

func (p *Parser) parseBody() []ast.Statement {
	var body []ast.Statement
	for {
		token := p.peek()
		if token.Type == lexer.RightBrace {
			p.eat()
			break
		}
		statement := p.parseStatement(token)
		if statement != nil {
			body = append(body, statement)
		}
		p.eatAll(lexer.Semicolon)
	}
	return body
}

func (p *Parser) parseStatement(token lexer.Token) ast.Statement {
	switch token.Type {
	case lexer.Return:
		return p.parseReturnStatement()
	default: // expression statement
		return p.parseExpressionStatement(token)
	}
}

func (p *Parser) parseReturnStatement() ast.ReturnStatement {
	p.eatOnly(lexer.Return)
	return ast.ReturnStatement{
		Expression: p.parseExpression(),
	}
}

func (p *Parser) parseExpressionStatement(token lexer.Token) ast.ExprStatement {
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
// primary        → NUMBER | STRING | ATOM | IDENTIFIER
//                | "(" expression ")" ;

func (p *Parser) parseExpression() ast.Expression {
	return p.parseMatch()
}

func (p *Parser) parseMatch() ast.Expression {
	left := p.parseEquality()
	// just if and not while because these are right-associative
	if p.matches(lexer.Equal) {
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
	} else if p.matches(lexer.ColonEqual) {
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
	for p.matches(lexer.EqualEqual, lexer.BangEqual) {
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
	for p.matches(lexer.Greater, lexer.GreaterEqual, lexer.Less, lexer.LessEqual) {
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
	for p.matches(lexer.Plus, lexer.Minus) {
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
	for p.matches(lexer.Slash, lexer.Star) {
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
	if p.matches(lexer.Minus, lexer.Plus) {
		op := p.eat()
		return ast.UnaryExpr{
			Operator: op.Value,
			Right:    p.parseUnary(),
		}
	}
	return p.parsePrimary()
}

func (p *Parser) parsePrimary() ast.Expression {
	token := p.eat()
	switch token.Type {
	case lexer.IntLiteral:
		return ast.IntLiteral{
			Value: p.parseInt(token),
		}
	case lexer.FloatLiteral:
		return ast.FloatLiteral{
			Value: p.parseFloat(token),
		}
	case lexer.Identifier:
		return ast.Identifier{
			Name: token,
		}
	case lexer.String:
		return ast.StringLiteral{
			Value: token.Value,
		}
	case lexer.Atom:
		return ast.AtomLiteral{
			Value: token.Value,
		}
	case lexer.LeftParen:
		expr := p.parseExpression()
		p.eatOnly(lexer.RightParen)
		return ast.ParenExpr{Expression: expr}
	default:
		p.error(fmt.Errorf("expected expression, got %s", token.Type.String()))
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
