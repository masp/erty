package parser

import (
	"fmt"

	"github.com/masp/ertylang/ast"
	"github.com/masp/ertylang/lexer"
	"github.com/masp/ertylang/token"
)

func (p *Parser) parseMatch(matchTok lexer.Token) ast.Expression {
	match := &ast.Match{MatchPos: matchTok.Pos}
	match.Value = p.parseExpression()
	lcurly := p.eatOnly(token.LCurlyBracket, "expected '{' to begin match clauses")
	if lcurly.Type != token.LCurlyBracket {
		end := p.advance(exprEnd)
		return &ast.BadExpr{From: lcurly.Pos, To: end.Pos}
	}
	match.Opener = lcurly.Pos

	// Parse each case statement until we hit a right curly
	for {
		if p.matches(token.EOF, token.RCurlyBracket) {
			rcurly := p.eatOnly(token.RCurlyBracket, "expected '}'")
			match.Closer = rcurly.Pos
			return match
		}
		match.Cases = append(match.Cases, p.parseCase())
	}
}

// parseCase parses a case expression like:
//
//	case A int: A + 10
func (p *Parser) parseCase() *ast.Case {
	caseClause := &ast.Case{}
	caseTok := p.eatOnly(token.Case, "expected 'case' at start of match clause")
	if caseTok.Type != token.Case {
		p.advance(caseStart)
		return caseClause
	}
	caseClause.CaseKeyword = caseTok.Pos

	caseClause.Pattern = p.parsePattern()
	caseClause.Colon = p.eatOnly(token.Colon, "expected ':' after pattern").Pos

	i := 0
	for !p.matches(token.EOF, token.RCurlyBracket, token.Case) {
		caseClause.Body = append(caseClause.Body, p.parseStatement(p.peek()))
		if p.matches(token.RCurlyBracket) {
			break // no semi needed
		}
		p.eatOnly(token.Semicolon, "expected ';' at end of statement")
		i += 1
	}
	return caseClause
}

// parsePattern is a more restricted behavior of parseExpression. For example, calling functions is not allowed, and in addition type information can be specified within the pattern
// after an identifier, like:
// - `A int`
// - [A, Tail int...]
func (p *Parser) parsePattern() ast.Expression {
	tok := p.eat()
	switch tok.Type {
	case token.Integer:
		return p.parseInt(tok)
	case token.Float:
		return p.parseFloat(tok)
	case token.Identifier:
		id := ast.NewIdent(tok)
		if _, isType := typeStart[p.peek().Type]; isType {
			return &ast.Field{Names: []*ast.Identifier{id}, Typ: p.parseType()}
		}
		return id
	case token.String:
		return ast.NewString(tok)
	case token.Atom:
		return ast.NewAtom(tok)
	case token.LParen: // parenthesis are tuples in pattern if they have a comma or are ()
		if p.matches(token.RParen) {
			closer := p.eat()
			return &ast.TupleLit{Opener: tok.Pos, Closer: closer.Pos}
		}

		first := p.parsePattern()
		if p.matches(token.Comma) {
			p.eat()
			rest := parseSequence(p, token.Comma, token.RParen, func(p *Parser) ast.Expression {
				return p.parsePattern()
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
			// Either it's an empty list (value) or a list type, or a pattern of list elements
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
			return p.parsePattern()
		})
		rbracket := p.eatOnly(token.RSquareBracket, "expected ']' to close list")
		return &ast.ListLiteral{Opener: tok.Pos, Elts: elts, Closer: rbracket.Pos}
	default:
		p.error(tok.Pos, fmt.Errorf("expected pattern, got %s", tok.Type.String()))
		to := p.advance(exprEnd)
		return &ast.BadExpr{From: tok.Pos, To: to.Pos}
	}
}
