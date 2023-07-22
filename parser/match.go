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

	for {
		if p.matches(token.EOF, token.RCurlyBracket, token.Case) {
			break
		}
		caseClause.Body = append(caseClause.Body, p.parseStatement(p.peek()))
		p.eatOnly(token.Semicolon, "expected ';' at end of statement")
	}
	return caseClause
}

// parsePattern is a much more restricted behavior of parseExpression. For example, operators like
// + are not allowed, and in addition type information can be specified within the pattern after an
// identifier, like:
// - `A int`
// - [A int, Tail int...]
func (p *Parser) parsePattern() ast.Expression {
	pattern := p.parseCall()
	switch expr := pattern.(type) {
	case *ast.ParenExpr:
		p.error(expr.Pos(), fmt.Errorf("parenthesis are not allowed in patterns"))
		return expr.Expression
	case *ast.CallExpr:
		p.error(expr.Pos(), fmt.Errorf("func calls are not allowed in patterns"))
		return &ast.BadExpr{From: expr.Pos(), To: expr.End()}
	}
	return pattern
}
