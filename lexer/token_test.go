package lexer

import (
	"strings"
	"testing"

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
				{Type: Identifier, Value: "foo"},
				{Type: EOF},
			},
		},
		{
			input: "120004 -102.4",
			expected: []Token{
				{Type: IntLiteral, Value: "120004"},
				{Type: Minus, Value: "-"},
				{Type: FloatLiteral, Value: "102.4"},
				{Type: EOF},
			},
		},
		{
			input: "foo bar",
			expected: []Token{
				{Type: Identifier, Value: "foo"},
				{Type: Identifier, Value: "bar"},
				{Type: EOF},
			},
		},
		{
			input: "foo.call()",
			expected: []Token{
				{Type: Identifier, Value: "foo"},
				{Type: Period, Value: "."},
				{Type: Identifier, Value: "call"},
				{Type: LeftParen, Value: "("},
				{Type: RightParen, Value: ")"},
				{Type: EOF},
			},
		},
		{
			input: "module foo",
			expected: []Token{
				{Type: Module, Value: "module"},
				{Type: Identifier, Value: "foo"},
				{Type: EOF},
			},
		},
		{
			input: "foo = X; bar = 123.5; z = 'atom'",
			expected: []Token{
				{Type: Identifier, Value: "foo"},
				{Type: Equal, Value: "="},
				{Type: Identifier, Value: "X"},
				{Type: Semicolon, Value: ";"},
				{Type: Identifier, Value: "bar"},
				{Type: Equal, Value: "="},
				{Type: FloatLiteral, Value: "123.5"},
				{Type: Semicolon, Value: ";"},
				{Type: Identifier, Value: "z"},
				{Type: Equal, Value: "="},
				{Type: Atom, Value: "atom"},
				{Type: EOF},
			},
		},
		{
			input: `func main() { return "hello world" }`,
			expected: []Token{
				{Type: Func, Value: "func"},
				{Type: Identifier, Value: "main"},
				{Type: LeftParen, Value: "("},
				{Type: RightParen, Value: ")"},
				{Type: LeftBrace, Value: "{"},
				{Type: Return, Value: "return"},
				{Type: String, Value: "hello world"},
				{Type: RightBrace, Value: "}"},
				{Type: EOF},
			},
		},
		// Test case for semicolon insertion where we two statements with newlines without explicit semicolons
		{
			input: `func main(a, b, c) {
				test = "hello world"
				test2 = "hello world 2"
			}`,
			expected: []Token{
				{Type: Func, Value: "func"},
				{Type: Identifier, Value: "main"},
				{Type: LeftParen, Value: "("},
				{Type: Identifier, Value: "a"},
				{Type: Comma, Value: ","},
				{Type: Identifier, Value: "b"},
				{Type: Comma, Value: ","},
				{Type: Identifier, Value: "c"},
				{Type: RightParen, Value: ")"},
				{Type: LeftBrace, Value: "{"},
				{Type: Identifier, Value: "test"},
				{Type: Equal, Value: "="},
				{Type: String, Value: "hello world"},
				{Type: Semicolon, Value: ";"},
				{Type: Identifier, Value: "test2"},
				{Type: Equal, Value: "="},
				{Type: String, Value: "hello world 2"},
				{Type: Semicolon, Value: ";"},
				{Type: RightBrace, Value: "}"},
				{Type: EOF},
			},
		},
		// Comparison tests
		{
			input: `foo == bar; foo != bar; foo < bar; foo > bar; foo <= bar; foo >= bar;`,
			expected: []Token{
				{Type: Identifier, Value: "foo"},
				{Type: EqualEqual, Value: "=="},
				{Type: Identifier, Value: "bar"},
				{Type: Semicolon, Value: ";"},
				{Type: Identifier, Value: "foo"},
				{Type: BangEqual, Value: "!="},
				{Type: Identifier, Value: "bar"},
				{Type: Semicolon, Value: ";"},
				{Type: Identifier, Value: "foo"},
				{Type: Less, Value: "<"},
				{Type: Identifier, Value: "bar"},
				{Type: Semicolon, Value: ";"},
				{Type: Identifier, Value: "foo"},
				{Type: Greater, Value: ">"},
				{Type: Identifier, Value: "bar"},
				{Type: Semicolon, Value: ";"},
				{Type: Identifier, Value: "foo"},
				{Type: LessEqual, Value: "<="},
				{Type: Identifier, Value: "bar"},
				{Type: Semicolon, Value: ";"},
				{Type: Identifier, Value: "foo"},
				{Type: GreaterEqual, Value: ">="},
				{Type: Identifier, Value: "bar"},
				{Type: Semicolon, Value: ";"},
				{Type: EOF},
			},
		},
	}

	for _, test := range tests {
		t.Run(test.input, func(t *testing.T) {
			lex := NewLexer(test.input)
			for _, expected := range test.expected {
				token := lex.NextToken()
				if token != expected {
					t.Errorf("Expected %s '%s', got %s '%s'", expected.Type, expected.Value, token.Type, token.Value)
				}
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
			expected: "unterminated string",
		},
	}

	for _, test := range tests {
		t.Run(test.input, func(t *testing.T) {
			lex := NewLexer(test.input)
			for {
				token := lex.NextToken()
				if token.Type == EOF {
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

	f.Fuzz(func(t *testing.T, input []byte) {
		lex := NewLexer(string(input))
		for {
			token := lex.NextToken()
			if token.Type == EOF {
				break
			}
		}
	})
}

func TestTokenTypeSTring(t *testing.T) {
	for i := EOF; i < LastTokenType; i += 1 {
		require.NotPanics(t, func() { _ = i.String() })
	}
}
