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
				{Type: token.Identifier, Value: "foo"},
				{Type: token.EOF},
			},
		},
		{
			input: "120004 -102.4",
			expected: []Token{
				{Type: token.Integer, Value: "120004"},
				{Type: token.Minus, Value: "-"},
				{Type: token.Float, Value: "102.4"},
				{Type: token.EOF},
			},
		},
		{
			input: "foo bar",
			expected: []Token{
				{Type: token.Identifier, Value: "foo"},
				{Type: token.Identifier, Value: "bar"},
				{Type: token.EOF},
			},
		},
		{
			input: "foo.call()",
			expected: []Token{
				{Type: token.Identifier, Value: "foo"},
				{Type: token.Period, Value: "."},
				{Type: token.Identifier, Value: "call"},
				{Type: token.LeftParen, Value: "("},
				{Type: token.RightParen, Value: ")"},
				{Type: token.EOF},
			},
		},
		{
			input: "module foo",
			expected: []Token{
				{Type: token.Module, Value: "module"},
				{Type: token.Identifier, Value: "foo"},
				{Type: token.EOF},
			},
		},
		{
			input: "foo = X; bar = 123.5; z = 'atom'",
			expected: []Token{
				{Type: token.Identifier, Value: "foo"},
				{Type: token.Equal, Value: "="},
				{Type: token.Identifier, Value: "X"},
				{Type: token.Semicolon, Value: ";"},
				{Type: token.Identifier, Value: "bar"},
				{Type: token.Equal, Value: "="},
				{Type: token.Float, Value: "123.5"},
				{Type: token.Semicolon, Value: ";"},
				{Type: token.Identifier, Value: "z"},
				{Type: token.Equal, Value: "="},
				{Type: token.Atom, Value: "atom"},
				{Type: token.EOF},
			},
		},
		{
			input: `func main() { return "hello world" }`,
			expected: []Token{
				{Type: token.Func, Value: "func"},
				{Type: token.Identifier, Value: "main"},
				{Type: token.LeftParen, Value: "("},
				{Type: token.RightParen, Value: ")"},
				{Type: token.LeftBrace, Value: "{"},
				{Type: token.Return, Value: "return"},
				{Type: token.String, Value: "hello world"},
				{Type: token.RightBrace, Value: "}"},
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
				{Type: token.Func, Value: "func"},
				{Type: token.Identifier, Value: "main"},
				{Type: token.LeftParen, Value: "("},
				{Type: token.Identifier, Value: "a"},
				{Type: token.Comma, Value: ","},
				{Type: token.Identifier, Value: "b"},
				{Type: token.Comma, Value: ","},
				{Type: token.Identifier, Value: "c"},
				{Type: token.RightParen, Value: ")"},
				{Type: token.LeftBrace, Value: "{"},
				{Type: token.Identifier, Value: "test"},
				{Type: token.Equal, Value: "="},
				{Type: token.String, Value: "hello world"},
				{Type: token.Semicolon, Value: "\n"},
				{Type: token.Identifier, Value: "test2"},
				{Type: token.Equal, Value: "="},
				{Type: token.String, Value: "hello world 2"},
				{Type: token.Semicolon, Value: "\n"},
				{Type: token.RightBrace, Value: "}"},
				{Type: token.EOF},
			},
		},
		// Comparison tests
		{
			input: `foo == bar; foo != bar; foo < bar; foo > bar; foo <= bar; foo >= bar;`,
			expected: []Token{
				{Type: token.Identifier, Value: "foo"},
				{Type: token.EqualEqual, Value: "=="},
				{Type: token.Identifier, Value: "bar"},
				{Type: token.Semicolon, Value: ";"},
				{Type: token.Identifier, Value: "foo"},
				{Type: token.BangEqual, Value: "!="},
				{Type: token.Identifier, Value: "bar"},
				{Type: token.Semicolon, Value: ";"},
				{Type: token.Identifier, Value: "foo"},
				{Type: token.Less, Value: "<"},
				{Type: token.Identifier, Value: "bar"},
				{Type: token.Semicolon, Value: ";"},
				{Type: token.Identifier, Value: "foo"},
				{Type: token.Greater, Value: ">"},
				{Type: token.Identifier, Value: "bar"},
				{Type: token.Semicolon, Value: ";"},
				{Type: token.Identifier, Value: "foo"},
				{Type: token.LessEqual, Value: "<="},
				{Type: token.Identifier, Value: "bar"},
				{Type: token.Semicolon, Value: ";"},
				{Type: token.Identifier, Value: "foo"},
				{Type: token.GreaterEqual, Value: ">="},
				{Type: token.Identifier, Value: "bar"},
				{Type: token.Semicolon, Value: ";"},
				{Type: token.EOF},
			},
		},
	}

	for _, test := range tests {
		t.Run(test.input, func(t *testing.T) {
			var prev Token
			lex := NewLexer("<test>", test.input)
			for _, expected := range test.expected {
				tok := lex.NextToken()
				require.Equal(t, expected.Type.String(), tok.Type.String(), "Expected token type to match. Prev: %s(%s)", prev.Type.String(), prev.Value)
				require.Equal(t, expected.Value, tok.Value, "Expected token value to match")
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
			lex := NewLexer("<test>", test.input)
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
		lex := NewLexer("<test>", string(input))
		for {
			tok := lex.NextToken()
			if tok.Type == token.EOF {
				break
			}
		}
	})
}
