// Package printer provides a pretty-printer for the AST. It is used by the formatting
// tool and for any visible error messages to the user about expression errors.
package printer

import (
	"bytes"
	"fmt"
	"io"

	"github.com/masp/garlang/ast"
	"github.com/masp/garlang/token"
)

var TokenTable = [...]string{
	token.Plus:           "+",
	token.Minus:          "-",
	token.Star:           "*",
	token.Slash:          "/",
	token.Equal:          "==",
	token.BangEqual:      "!=",
	token.Less:           "<",
	token.LessEqual:      "<=",
	token.Greater:        ">",
	token.GreaterEqual:   ">=",
	token.Bang:           "!",
	token.EqualEqual:     "==",
	token.ColonEqual:     ":=",
	token.Colon:          ":",
	token.Semicolon:      ";",
	token.Comma:          ",",
	token.LParen:         "(",
	token.RParen:         ")",
	token.LCurlyBracket:  "{",
	token.RCurlyBracket:  "}",
	token.LSquareBracket: "[",
	token.RSquareBracket: "]",
	token.Period:         ".",
	token.Func:           "func",
	token.Return:         "return",
}

type Config struct{}

type printer struct {
	file    *token.File
	out     bytes.Buffer // write internally to a buffer and then write the whole thing
	errlist token.ErrorList
}

func Print(file *token.File, node ast.Node) (string, error) {
	var out bytes.Buffer
	cfg := &Config{}
	err := cfg.Fprint(&out, file, node)
	if err != nil {
		return "", err
	}
	return out.String(), nil
}

func (c *Config) Fprint(w io.Writer, file *token.File, node ast.Node) error {
	p := &printer{}
	p.printNode(node)
	_, err := w.Write(p.out.Bytes())
	if err != nil {
		return err
	}
	return p.errlist.Err()
}

func (p *printer) error(node ast.Node, format string, args ...any) {
	p.errlist.Add(p.file.Position(node.Pos()), fmt.Errorf(format, args...))
}

func (p *printer) printNode(node ast.Node) {
	switch node := node.(type) {
	case ast.Expression:
		p.printExpr(node)
	default:
		panic(fmt.Errorf("printer does not support nodes of type %T", node))
	}
}

func (p *printer) write(data ...any) {
	for _, d := range data {
		switch d := d.(type) {
		case string:
			p.out.WriteString(d)
		case token.Type:
			p.out.WriteString(TokenTable[d])
		case byte:
			p.out.WriteByte(d)
		}
	}
}

func (p *printer) printExpr(expr ast.Expression) {
	switch expr := expr.(type) {
	case *ast.Identifier:
		p.write(expr.Name)
	case *ast.BinaryExpr:
		p.printExpr(expr.Left)
		p.write(" ", expr.Op, " ")
		p.printExpr(expr.Right)
	case *ast.UnaryExpr:
		p.write(expr.Op)
		p.printExpr(expr.Right)
	case *ast.CallExpr:
		p.printExpr(expr.Fun)
		p.write(token.LParen)
		for i, arg := range expr.Args {
			if i > 0 {
				p.write(token.Comma, " ")
			}
			p.printExpr(arg)
		}
		p.write(token.RParen)
	case *ast.IntLiteral:
		p.write(expr.Lit)
	case *ast.FloatLiteral:
		p.write(expr.Lit)
	case *ast.StringLiteral:
		p.write(expr.Lit)
	case *ast.AtomLiteral:
		p.write("'", expr.Value, "'")
	case *ast.DotExpr:
		p.printExpr(expr.X)
		p.write(token.Period, expr.Attr.Name)
	default:
		panic(fmt.Errorf("printer does not support expressions of type %T", expr))
	}
}
