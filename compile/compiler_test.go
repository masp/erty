package compile

import (
	"bytes"
	"testing"

	"github.com/masp/garlang/core"
	"github.com/masp/garlang/lexer"
	"github.com/masp/garlang/parse"
	"github.com/sebdah/goldie/v2"
	"github.com/stretchr/testify/require"
)

func TestCompileModule(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			input:    `module mod; export func a() { return 1 }`,
			expected: "mod.core",
		},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			lex := lexer.NewLexer(tt.input)
			toks := lex.All()
			if lex.HasErrors() {
				t.Fatalf("lexer errors: %v", lex.Errors())
			}

			mod, err := parse.Module(toks)
			if err != nil {
				t.Fatalf("parse program: %v", err)
			}

			compiled, err := New().CompileModule(mod)
			require.NoError(t, err)

			var out bytes.Buffer
			core.NewPrinter(&out).PrintModule(compiled)
			g := goldie.New(t)
			g.Assert(t, tt.expected, out.Bytes())
		})
	}

}

func TestCompileFunc(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			input:    `func a() {return 'a'}`,
			expected: "a.core",
		},
		{
			input:    `func call() { return erlang.module_info('b') }`,
			expected: "call.core",
		},
	}

	for _, test := range tests {
		t.Run(test.input, func(t *testing.T) {
			lex := lexer.NewLexer(test.input)
			toks := lex.All()
			if lex.HasErrors() {
				t.Fatalf("lexer errors: %v", lex.Errors())
			}

			fn, err := parse.Function(toks)
			if err != nil {
				t.Fatalf("parse program: %v", err)
			}

			compiled, err := New().CompileFunction(fn)
			require.NoError(t, err)

			var out bytes.Buffer
			core.NewPrinter(&out).PrintFunc(compiled)
			g := goldie.New(t)
			g.Assert(t, test.expected, out.Bytes())
		})
	}
}
