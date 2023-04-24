package parse

import (
	"fmt"

	"github.com/masp/garlang/ast"
	"github.com/masp/garlang/lexer"
	"github.com/masp/garlang/token"
)

func Module(filename string, src string) (mod *ast.Module, err error) {
	lex := lexer.NewLexer(filename, src)
	tokens := lex.All()
	if lex.HasErrors() {
		return nil, lex.Errors()
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

	mod = parser.parseModuleHeader(lex.File())
	for {
		tok := parser.peek()
		if tok.Type == token.EOF {
			break
		}

		switch tok.Type {
		case token.Func, token.Export:
			mod.Decls = append(mod.Decls, parser.parseFunction())
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
	return mod, err
}

func Function(src string) (function *ast.FuncDecl, err error) {
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
