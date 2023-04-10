package compile

import (
	"bytes"
	"errors"
	"testing"

	"github.com/masp/garlang/lexer"
	"github.com/masp/garlang/parse"
	"github.com/sebdah/goldie/v2"
)

func TestEmitFunc(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			input:    `module "test_a"; func a() {return 'a'}`,
			expected: "test_a.core",
		},
	}

	for _, test := range tests {
		t.Run(test.input, func(t *testing.T) {
			lex := lexer.NewLexer(test.input)
			toks := lex.All()
			if lex.HasErrors() {
				t.Fatalf("lexer errors: %v", errors.Join(lex.Errors()...))
			}

			fn, err := parse.Module(toks)
			if err != nil {
				t.Fatalf("parse program: %v", err)
			}

			var out bytes.Buffer
			New(&out).EmitModule(fn)
			g := goldie.New(t)
			g.Assert(t, test.expected, out.Bytes())
		})
	}
}
