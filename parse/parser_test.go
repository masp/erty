package parse

import (
	"bytes"
	"testing"

	"github.com/sebdah/goldie/v2"
	"github.com/stretchr/testify/assert"
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
			input:       "func call() { mod.fn(1); local(2) }",
			expectedAst: "call.ast",
		},
		{
			input:       "func recursive() { mod.fn(1).fn(2).fn(3) }",
			expectedAst: "recursive.ast",
		},
	}
	for _, test := range tests {
		t.Run(test.input, func(t *testing.T) {
			fn, err := Function(test.input)
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
			input: `module test
				export func expr() {
					test = "hello world"
					a = 3 + 5
				}`,
			expectedAst: "module.ast",
		},
	}
	for _, test := range tests {
		t.Run(test.input, func(t *testing.T) {
			mod, err := Module("<test>", test.input)
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

func TestParseFail(t *testing.T) {
	tests := []struct {
		input   string
		wantErr string
	}{
		{
			input:   "module abc; func foo() {",
			wantErr: "unexpected end of file",
		},
		{
			input:   "module abc; fn foo() { return 1 }",
			wantErr: `expected func, got "fn" (Identifier)`,
		},
	}
	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			_, err := Module("<test>", tt.input)
			if err == nil {
				t.Fatalf("expected error")
			}
			assert.ErrorContainsf(t, err, tt.wantErr, "expected error %q, got %q", tt.wantErr, err.Error())
		})
	}

}
