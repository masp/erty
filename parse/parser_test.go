package parse

import (
	"bytes"
	"errors"
	"testing"

	"github.com/masp/garlang/lexer"
	"github.com/sebdah/goldie/v2"
)

// TestParseFunc will take an input func decl, print it to a string, and then compare that matches what's
// expected in testdata/.
func TestParseFunc(t *testing.T) {
	tests := []struct {
		input       string
		expectedAst string
	}{
		{
			input: `func expr() {
				test = "hello world"
				a = 3 + 5
			}`,
			expectedAst: "expr.ast",
		},
		{
			input:       "func foo() {}",
			expectedAst: "func_foo.ast",
		},
		{
			input:       "func ret() { return -b }",
			expectedAst: "return.ast",
		},
		{
			input:       "func params(a, b, c) {}",
			expectedAst: "params.ast",
		},
		{
			input:       "func call() { mod.func(1); local(2) }",
			expectedAst: "call.ast",
		},
	}
	for _, test := range tests {
		t.Run(test.input, func(t *testing.T) {
			lex := lexer.NewLexer(test.input)
			toks := lex.All()
			if lex.HasErrors() {
				t.Fatalf("lexer errors: %v", errors.Join(lex.Errors()...))
			}

			fn, err := Function(toks)
			if err != nil {
				t.Fatalf("parse program: %v", err)
			}

			var out bytes.Buffer
			printer := NewPrinter(&out)
			printer.Print(fn)
			g := goldie.New(t)
			g.Assert(t, test.expectedAst, out.Bytes())
		})
	}
}

func TestParseModule(t *testing.T) {
	tests := []struct {
		input       string
		expectedAst string
	}{
		{
			input: `module "test"
				func expr() {
					test = "hello world"
					a = 3 + 5
				}`,
			expectedAst: "module.ast",
		},
	}
	for _, test := range tests {
		t.Run(test.input, func(t *testing.T) {
			lex := lexer.NewLexer(test.input)
			toks := lex.All()
			if lex.HasErrors() {
				t.Fatalf("lexer errors: %v", errors.Join(lex.Errors()...))
			}

			mod, err := Module(toks)
			if err != nil {
				t.Fatalf("parse program: %v", err)
			}

			var out bytes.Buffer
			printer := NewPrinter(&out)
			printer.Print(mod)
			g := goldie.New(t)
			g.Assert(t, test.expectedAst, out.Bytes())
		})
	}
}
