package parser

import (
	"fmt"

	"github.com/masp/ertylang/ast"
	"github.com/masp/ertylang/lexer"
	"github.com/masp/ertylang/token"
)

type Options struct {
	// DeclarationOnly will parse only the declaration of a function, not the body. If a body
	// exists, it is considered an error. This mode is used for .d.ert files.
	DeclarationOnly bool
}

func ParseModule(filename string, src []byte, opts *Options) (mod *ast.Module, err error) {
	if opts == nil {
		opts = &Options{}
	}
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

	parser.parseModuleHeader(mod, lex.File())
	if err != nil {
		// exit early if module header is bad (likely not our file)
		return mod, err
	}

	for {
		tok := parser.peek()
		if tok.Type == token.EOF {
			break
		}

		switch tok.Type {
		case token.Func:
			fn := parser.parseFunctionHeader()
			if fn, ok := fn.(*ast.FuncDecl); ok && !opts.DeclarationOnly {
				parser.parseFunctionBody(fn)
			}
			mod.Decls = append(mod.Decls, fn)
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

func ParseFunc(src []byte) (file *token.File, function *ast.FuncDecl, err error) {
	lex := lexer.NewLexer("<string>", src)
	tokens := lex.All()
	if lex.HasErrors() {
		return nil, nil, lex.Errors()
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
	fn := parser.parseFunctionHeader()
	if fn, ok := fn.(*ast.FuncDecl); ok {
		parser.parseFunctionBody(fn)
		return parser.file, fn, err
	}
	return nil, nil, err
}

func ParseExpr(program string) (file *token.File, result ast.Expression, err error) {
	lex := lexer.NewLexer("<string>", []byte(program))
	tokens := lex.All()
	if lex.HasErrors() {
		return nil, nil, lex.Errors()
	}

	file = lex.File()
	parser := &Parser{
		tokens: tokens,
		file:   lex.File(),
	}
	defer func() {
		errlist := parser.catchErrors()
		errlist.Sort()
		err = errlist.Err()
	}()
	return file, parser.parseExpression(), nil
}
