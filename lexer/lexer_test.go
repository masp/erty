package lexer

import (
	"strings"
	"testing"

	"github.com/masp/garlang/token"
	"github.com/stretchr/testify/require"
)

func TestLex(t *testing.T) {
	tests := []struct {
		input    string
		expected []Token
	}{
		{
			input: "foo",
			expected: []Token{
				{Type: token.Identifier, Lit: "foo"},
				{Type: token.EOF},
			},
		},
		{
			input: "120004 -102.4",
			expected: []Token{
				{Type: token.Integer, Lit: "120004"},
				{Type: token.Minus, Lit: "-"},
				{Type: token.Float, Lit: "102.4"},
				{Type: token.EOF},
			},
		},
		{
			input: "foo bar",
			expected: []Token{
				{Type: token.Identifier, Lit: "foo"},
				{Type: token.Identifier, Lit: "bar"},
				{Type: token.EOF},
			},
		},
		{
			input: "foo.call()",
			expected: []Token{
				{Type: token.Identifier, Lit: "foo"},
				{Type: token.Period, Lit: "."},
				{Type: token.Identifier, Lit: "call"},
				{Type: token.LParen, Lit: "("},
				{Type: token.RParen, Lit: ")"},
				{Type: token.EOF},
			},
		},
		{
			input: "module foo",
			expected: []Token{
				{Type: token.Module, Lit: "module"},
				{Type: token.Identifier, Lit: "foo"},
				{Type: token.EOF},
			},
		},
		{
			input: "foo = X; bar = 123.5; z = 'atom'",
			expected: []Token{
				{Type: token.Identifier, Lit: "foo"},
				{Type: token.Equal, Lit: "="},
				{Type: token.Identifier, Lit: "X"},
				{Type: token.Semicolon, Lit: ";"},
				{Type: token.Identifier, Lit: "bar"},
				{Type: token.Equal, Lit: "="},
				{Type: token.Float, Lit: "123.5"},
				{Type: token.Semicolon, Lit: ";"},
				{Type: token.Identifier, Lit: "z"},
				{Type: token.Equal, Lit: "="},
				{Type: token.Atom, Lit: "atom"},
				{Type: token.EOF},
			},
		},
		{
			input: `func main() { return "hello world" }`,
			expected: []Token{
				{Type: token.Func, Lit: "func"},
				{Type: token.Identifier, Lit: "main"},
				{Type: token.LParen, Lit: "("},
				{Type: token.RParen, Lit: ")"},
				{Type: token.LCurlyBracket, Lit: "{"},
				{Type: token.Return, Lit: "return"},
				{Type: token.String, Lit: "hello world"},
				{Type: token.RCurlyBracket, Lit: "}"},
				{Type: token.EOF},
			},
		},
		// Test case for semicolon insertion where we two statements with newlines without explicit semicolons
		{
			input: `func main(a, b, c) {
				test = "hello world"
				test2 = "hello world 2"
			}`,
			expected: []Token{
				{Type: token.Func, Lit: "func"},
				{Type: token.Identifier, Lit: "main"},
				{Type: token.LParen, Lit: "("},
				{Type: token.Identifier, Lit: "a"},
				{Type: token.Comma, Lit: ","},
				{Type: token.Identifier, Lit: "b"},
				{Type: token.Comma, Lit: ","},
				{Type: token.Identifier, Lit: "c"},
				{Type: token.RParen, Lit: ")"},
				{Type: token.LCurlyBracket, Lit: "{"},
				{Type: token.Identifier, Lit: "test"},
				{Type: token.Equal, Lit: "="},
				{Type: token.String, Lit: "hello world"},
				{Type: token.Semicolon, Lit: "\n"},
				{Type: token.Identifier, Lit: "test2"},
				{Type: token.Equal, Lit: "="},
				{Type: token.String, Lit: "hello world 2"},
				{Type: token.Semicolon, Lit: "\n"},
				{Type: token.RCurlyBracket, Lit: "}"},
				{Type: token.EOF},
			},
		},
		// Comparison tests
		{
			input: `foo == bar; foo != bar; foo < bar; foo > bar; foo <= bar; foo >= bar;`,
			expected: []Token{
				{Type: token.Identifier, Lit: "foo"},
				{Type: token.EqualEqual, Lit: "=="},
				{Type: token.Identifier, Lit: "bar"},
				{Type: token.Semicolon, Lit: ";"},
				{Type: token.Identifier, Lit: "foo"},
				{Type: token.BangEqual, Lit: "!="},
				{Type: token.Identifier, Lit: "bar"},
				{Type: token.Semicolon, Lit: ";"},
				{Type: token.Identifier, Lit: "foo"},
				{Type: token.Less, Lit: "<"},
				{Type: token.Identifier, Lit: "bar"},
				{Type: token.Semicolon, Lit: ";"},
				{Type: token.Identifier, Lit: "foo"},
				{Type: token.Greater, Lit: ">"},
				{Type: token.Identifier, Lit: "bar"},
				{Type: token.Semicolon, Lit: ";"},
				{Type: token.Identifier, Lit: "foo"},
				{Type: token.LessEqual, Lit: "<="},
				{Type: token.Identifier, Lit: "bar"},
				{Type: token.Semicolon, Lit: ";"},
				{Type: token.Identifier, Lit: "foo"},
				{Type: token.GreaterEqual, Lit: ">="},
				{Type: token.Identifier, Lit: "bar"},
				{Type: token.Semicolon, Lit: ";"},
				{Type: token.EOF},
			},
		},
	}

	for _, test := range tests {
		t.Run(test.input, func(t *testing.T) {
			var prev Token
			lex := NewLexer("<test>", []byte(test.input))
			for _, expected := range test.expected {
				tok := lex.NextToken()
				require.Equal(t, expected.Type.String(), tok.Type.String(), "Expected token type to match. Prev: %s(%s)", prev.Type.String(), prev.Lit)
				require.Equal(t, expected.Lit, tok.Lit, "Expected token value to match")
				prev = tok
			}
		})
	}
}

func TestLexErrors(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			input:    "func main() { test = \"hello world }",
			expected: "<test>:1:22: unterminated string",
		},
		{
			input:    "'0",
			expected: "<test>:1:1: unterminated string",
		},
	}

	for _, test := range tests {
		t.Run(test.input, func(t *testing.T) {
			lex := NewLexer("<test>", []byte(test.input))
			for {
				tok := lex.NextToken()
				if tok.Type == token.EOF {
					break
				}
			}
			if !lex.HasErrors() {
				t.Fatal("Expected lexer to have errors")
			}

			var errText []string
			for _, err := range lex.Errors() {
				errText = append(errText, err.Error())
			}
			require.Equal(t, test.expected, strings.Join(errText, ";"))
		})
	}
}

func FuzzLex(f *testing.F) {
	f.Add([]byte("foo"))
	f.Add([]byte("foo bar"))
	f.Add([]byte("foo = X; bar = 123.5"))
	f.Add([]byte(`func main() { test = "hello world" }`))
	f.Add([]byte(`!!!`))
	f.Add([]byte(`"0`))
	f.Add([]byte(`'0`))

	f.Fuzz(func(t *testing.T, input []byte) {
		lex := NewLexer("<test>", input)
		for {
			tok := lex.NextToken()
			if tok.Type == token.EOF {
				break
			}
		}
	})
}
