package parser

import (
	"fmt"

	"github.com/masp/ertylang/ast"
	"github.com/masp/ertylang/lexer"
	"github.com/masp/ertylang/token"
)

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

func (p *Parser) parseType() ast.Expression {
	left := p.parseTypePrimary()
	for p.matches(token.Pipe) {
		op := p.eat()
		right := p.parseTypePrimary()
		left = &ast.BinaryExpr{
			Left:  left,
			Op:    op.Type,
			OpPos: op.Pos,
			Right: right,
		}
	}
	return left
}

// parseType only parses types expressions. Types can either be identifiers,
// or one of the built in types like lists, tuples, binaries, etc...
func (p *Parser) parseTypePrimary() ast.Expression {
	tok := p.eat()
	if _, validType := typeStart[tok.Type]; !validType {
		to := p.advance(exprEnd)
		p.error(tok.Pos, fmt.Errorf("expected type, got %s", tok.Lit))
		return &ast.BadExpr{From: tok.Pos, To: to.Pos}
	}

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
	case token.Atom: // 'fixed_value'
		// Atoms are a unique type because they are a "literal", and not a description of literals
		// Because atom's are often used as tags for types, however, they are allowed.
		// It is not legeal to use any other literal besides an atom.
		return ast.NewAtom(tok)
	case token.Func: // func(...) ...
		params := p.parseParams()

		var retType ast.Expression
		if _, isType := typeStart[p.peek().Type]; isType {
			// If the following token could be a type, it must be our return type
			retType = p.parseType()
		}
		return &ast.FuncDecl{
			Func:       tok.Pos,
			Parameters: params,
			ReturnType: retType,
		}
	case token.LParen: // tuple (...,)
		return p.parseTupleType(tok)
	case token.LSquareBracket: // []type
		rbrack := p.eatOnly(token.RSquareBracket, "expected ']' after '[' for list type")
		if rbrack.Type != token.RSquareBracket {
			return &ast.BadExpr{From: tok.Pos, To: rbrack.Pos}
		}
		eltType := p.parseType()
		return &ast.ListType{Lbrack: tok.Pos, Elt: eltType, Rbrack: rbrack.Pos}
	case token.Enum:
		lcurly := p.eatOnly(token.LCurlyBracket, "expected '{' after 'enum' for enum type")
		if lcurly.Type != token.LCurlyBracket {
			return &ast.BadExpr{From: tok.Pos, To: lcurly.Pos}
		}
		// Parse each case of the enum
		enumT := &ast.EnumType{EnumPos: tok.Pos, Opener: lcurly.Pos}
		enumT.Cases = parseSequence(p, token.Semicolon, token.RCurlyBracket, func(p *Parser) ast.Expression {
			return p.parseType()
		})
		enumT.Closer = p.eatOnly(token.RCurlyBracket, "expected '}' to close enum").Pos
		return enumT
	default:
		panic("unreachable (typeStart should catch before this is reached)")
	}
}

// parseTupleType parses a tuple of the form `tuple[<fieldlist>]` and returns
// the resulting expression. A tuple can look like:
// - tuple[] (only empty tuple {} allowed)
// - tuple[int, int] (2 ints)
func (p *Parser) parseTupleType(opener lexer.Token) ast.Expression {
	fields := parseSequence(p, token.Comma, token.RParen, func(p *Parser) ast.Expression {
		return p.parseType()
	})
	rbracket := p.eatOnly(token.RParen, "expected ')' after tuple type list")
	return &ast.TupleType{
		Opener: opener.Pos,
		Elts:   fields,
		Closer: rbracket.Pos,
	}
}
