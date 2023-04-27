package parse

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/masp/garlang/ast"
	"github.com/masp/garlang/token"
	"github.com/sebdah/goldie/v2"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
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
				test = 'hello'
				a = 3 + 5
			}`,
			expectedAst: "expr.ast",
		},
		// empty statement (many semis)
		{
			input:       "func empty() { ; ; ; ; ; ; ; ; ; }",
			expectedAst: "empty.ast",
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
			fn, err := Function([]byte(test.input))
			if err != nil {
				t.Fatalf("parse program: %v", err)
			}

			var out bytes.Buffer
			ast.Fprint(&out, nil, fn, ast.NotNilFilter)
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
			mod, err := Module("<test>", []byte(test.input))
			if err != nil {
				t.Fatalf("parse program: %v", err)
			}

			var out bytes.Buffer
			ast.Fprint(&out, mod.File, mod, ast.NotNilFilter)
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
			_, err := Module("<test>", []byte(tt.input))
			if err == nil {
				t.Fatalf("expected error")
			}
			assert.ErrorContainsf(t, err, tt.wantErr, "expected error %q, got %q", tt.wantErr, err.Error())
		})
	}

}

func TestParseBadNodes(t *testing.T) {
	tests := []struct {
		input       string
		expectedAst string
	}{
		{
			input: `module test
fn bad() { return 1 }
func hello() { return 'abc' }`,
			expectedAst: "badfunc.ast",
		},
		{
			input: `module test
func bad() {
	go home {

	}
	a = 12
}`,
			expectedAst: "badstmt.ast",
		},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			mod, err := Module("<test>", []byte(tt.input))
			require.Error(t, err, "there should be at least 1 error in the program")
			require.NotNil(t, mod)

			var out bytes.Buffer
			ast.Fprint(&out, mod.File, mod, ast.NotNilFilter)
			g := goldie.New(t)
			g.Assert(t, tt.expectedAst, out.Bytes())
		})
	}

}

func TestAllErrors(t *testing.T) {
	tests := []struct {
		input        string
		expectedErrs string
	}{
		{
			input: `module test


fn bad() { return 1 }
`,
			expectedErrs: "badfunc.errors",
		},
		{
			input:        `module test; func (){}`,
			expectedErrs: "badid.errors",
		},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			mod, err := Module("<test>", []byte(tt.input))
			require.Error(t, err, "there should be at least 1 error in the program")
			require.NotNil(t, mod)

			errlist := err.(token.ErrorList)

			var out bytes.Buffer
			for _, err := range errlist {
				fmt.Fprintf(&out, "%s: %v\n", err.Pos, err.Msg)
			}
			g := goldie.New(t, goldie.WithFixtureDir("testdata/errors"))
			g.Assert(t, tt.expectedErrs, out.Bytes())
		})
	}
}
