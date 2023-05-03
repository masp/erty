package parse

import (
	"fmt"

	"github.com/masp/garlang/ast"
	"github.com/masp/garlang/lexer"
	"github.com/masp/garlang/token"
)

func Module(filename string, src []byte) (mod *ast.Module, err error) {
	lex := lexer.NewLexer(filename, src)
	mod = &ast.Module{File: lex.File()}
	tokens := lex.All()
	if lex.HasErrors() {
		err = lex.Errors()
		return
	}

	parser := &Parser{
		file:   lex.File(),
		tokens: tokens,
	}

	defer func() {
		errlist := parser.catchErrors()
		errlist.Sort()
		if errlist.Len() > 0 {
			err = errlist.Err()
		}
	}()

	err = parser.parseModuleHeader(mod, lex.File())
	if err != nil {
		// exit early if module header is bad (likely not our file)
		return mod, err
	}

	if !parser.matches(token.EOF) {
		parser.eatOnly(token.Semicolon, "expected ';' after module declaration")
	}

	for {
		tok := parser.peek()
		if tok.Type == token.EOF {
			break
		}

		switch tok.Type {
		case token.Func, token.Export:
			mod.Decls = append(mod.Decls, parser.parseFunction())
			if !parser.matches(token.EOF) {
				parser.eatOnly(token.Semicolon, "expected ';' after function declaration")
			}
		case token.TypeKeyword:
			mod.Decls = append(mod.Decls, parser.parseTypeDecl())
			if !parser.matches(token.EOF) {
				parser.eatOnly(token.Semicolon, "expected ';' after type declaration")
			}
		case token.Semicolon:
			parser.eat()
			continue
		default:
			from := parser.eat() // skip next token
			parser.error(tok.Pos, fmt.Errorf("expected func, got %q (%s)", tok.Lit, tok.Type.String()))
			to := parser.advance(declStart)
			mod.Decls = append(mod.Decls, &ast.BadDecl{From: from.Pos, To: to.Pos})
		}
	}
	return
}

func Function(src []byte) (function *ast.FuncDecl, err error) {
	lex := lexer.NewLexer("<string>", src)
	tokens := lex.All()
	if lex.HasErrors() {
		return nil, lex.Errors()
	}

	parser := &Parser{
		tokens: tokens,
		file:   lex.File(),
	}
	defer func() {
		errlist := parser.catchErrors()
		errlist.Sort()
		if errlist.Len() > 0 {
			err = errlist.Err()
		}
	}()
	fn := parser.parseFunction()
	if fn, ok := fn.(*ast.FuncDecl); ok {
		return fn, err
	}
	return nil, err
}
