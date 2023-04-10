package lexer

import (
	"fmt"
	"unicode"
)

type TokenType int

const (
	EOF TokenType = iota
	// Literlas/Terminal types
	Atom
	Identifier
	String
	IntLiteral
	FloatLiteral

	// Comparisons
	Bang
	EqualEqual
	BangEqual
	LessEqual
	Less
	GreaterEqual
	Greater

	// Math operators
	Plus
	Minus
	Slash
	Star

	// Other
	Period
	Equal
	ColonEqual
	Semicolon
	LeftParen
	RightParen
	LeftBrace
	RightBrace
	Comma

	// Keywords
	Func
	Return
	Module

	LastTokenType
)

type Token struct {
	Type  TokenType
	Value string
}

// String maps each of the token types into a human readable name
func (t TokenType) String() string {
	switch t {
	case EOF:
		return "EOF"
	case Atom:
		return "Atom"
	case Identifier:
		return "Identifier"
	case String:
		return "String"
	case IntLiteral:
		return "IntLiteral"
	case FloatLiteral:
		return "FloatLiteral"
	case Bang:
		return "Bang"
	case EqualEqual:
		return "EqualEqual"
	case BangEqual:
		return "BangEqual"
	case LessEqual:
		return "LessEqual"
	case Less:
		return "Less"
	case GreaterEqual:
		return "GreaterEqual"
	case Greater:
		return "Greater"
	case Plus:
		return "Plus"
	case Minus:
		return "Minus"
	case Slash:
		return "Slash"
	case Star:
		return "Star"
	case Equal:
		return "Equal"
	case ColonEqual:
		return "ColonEqual"
	case Period:
		return "Period"
	case Semicolon:
		return "Semicolon"
	case Func:
		return "Func"
	case LeftParen:
		return "LeftParen"
	case RightParen:
		return "RightParen"
	case LeftBrace:
		return "LeftBrace"
	case RightBrace:
		return "RightBrace"
	case Return:
		return "Return"
	case Module:
		return "Module"
	case Comma:
		return "Comma"
	default:
		panic(fmt.Sprintf("unknown token type %d", t))
	}
}

type Lexer struct {
	input     string
	pos       int
	prevToken Token

	errors []error
}

func (l *Lexer) error(err error) {
	l.errors = append(l.errors, err)
}

func (l *Lexer) Errors() []error {
	return l.errors
}

func (l *Lexer) HasErrors() bool {
	return len(l.errors) > 0
}

// insertSemi returns if one of the following exists:
// an identifier
// an integer, floating-point, imaginary, rune, or string literal
// one of the keywords break, continue, fallthrough, or return
// one of the operators and delimiters ++, --, ), ], or }
func insertSemi(prevToken Token) bool {
	switch prevToken.Type {
	case Identifier, String, RightParen, RightBrace, IntLiteral, FloatLiteral:
		return true
	}
	return false
}

func NewLexer(input string) *Lexer {
	return &Lexer{input: input}
}

func (l *Lexer) All() []Token {
	var tokens []Token
	for {
		tok := l.NextToken()
		if tok.Type == EOF {
			break
		}
		tokens = append(tokens, tok)
	}
	return tokens
}

func (l *Lexer) NextToken() (tok Token) {
	defer func() {
		l.prevToken = tok
	}()
	if l.HasErrors() {
		return Token{Type: EOF}
	}

	l.skipWhitespace()

	if l.pos >= len(l.input) {
		return Token{Type: EOF}
	}

	switch {
	case l.input[l.pos] == '+':
		l.pos++
		return Token{Type: Plus, Value: "+"}
	case l.input[l.pos] == '-':
		l.pos++
		return Token{Type: Minus, Value: "-"}
	case l.input[l.pos] == '*':
		l.pos++
		return Token{Type: Star, Value: "*"}
	case l.input[l.pos] == '/':
		l.pos++
		return Token{Type: Slash, Value: "/"}
	case l.pos+2 < len(l.input) && l.input[l.pos:l.pos+2] == "!=":
		l.pos += 2
		return Token{Type: BangEqual, Value: "!="}
	case l.pos+2 < len(l.input) && l.input[l.pos:l.pos+2] == "==":
		l.pos += 2
		return Token{Type: EqualEqual, Value: "=="}
	case l.pos+2 < len(l.input) && l.input[l.pos:l.pos+2] == "<=":
		l.pos += 2
		return Token{Type: LessEqual, Value: "<="}
	case l.pos+2 < len(l.input) && l.input[l.pos:l.pos+2] == ">=":
		l.pos += 2
		return Token{Type: GreaterEqual, Value: ">="}
	case l.pos+2 < len(l.input) && l.input[l.pos:l.pos+2] == ":=":
		l.pos += 2
		return Token{Type: ColonEqual, Value: ":="}
	case l.input[l.pos] == '=':
		l.pos++
		return Token{Type: Equal, Value: "="}
	case l.input[l.pos] == '!':
		l.pos++
		return Token{Type: Bang, Value: "!"}
	case l.input[l.pos] == '<':
		l.pos++
		return Token{Type: Less, Value: "<"}
	case l.input[l.pos] == '>':
		l.pos++
		return Token{Type: Greater, Value: ">"}
	case l.input[l.pos] == ';':
		l.pos++
		return Token{Type: Semicolon, Value: ";"}
	case l.input[l.pos] == '(':
		l.pos++
		return Token{Type: LeftParen, Value: "("}
	case l.input[l.pos] == ')':
		l.pos++
		return Token{Type: RightParen, Value: ")"}
	case l.input[l.pos] == '{':
		l.pos++
		return Token{Type: LeftBrace, Value: "{"}
	case l.input[l.pos] == '}':
		l.pos++
		return Token{Type: RightBrace, Value: "}"}
	case l.input[l.pos] == ',':
		l.pos++
		return Token{Type: Comma, Value: ","}
	case l.input[l.pos] == '.':
		l.pos++
		return Token{Type: Period, Value: "."}
	case l.pos+4 < len(l.input) && l.input[l.pos:l.pos+4] == "func":
		l.pos += 4
		return Token{Type: Func, Value: "func"}
	case l.pos+6 < len(l.input) && l.input[l.pos:l.pos+6] == "return":
		l.pos += 6
		return Token{Type: Return, Value: "return"}
	case l.pos+6 < len(l.input) && l.input[l.pos:l.pos+6] == "module":
		l.pos += 6
		return Token{Type: Module, Value: "module"}
	case unicode.IsLetter(rune(l.input[l.pos])):
		return Token{Type: Identifier, Value: l.readIdentifier()}
	case unicode.IsDigit(rune(l.input[l.pos])):
		return l.readNumber()
	case l.input[l.pos] == '"':
		return Token{Type: String, Value: l.readString('"')}
	case l.input[l.pos] == '\'':
		return Token{Type: Atom, Value: l.readString('\'')}
	case l.input[l.pos] == '\n':
		if insertSemi(l.prevToken) {
			return Token{Type: Semicolon, Value: ";"}
		}
		l.skipAllWhitespace()
		return l.NextToken()
	default:
		l.error(fmt.Errorf("invalid token at position %d", l.pos))
		return Token{Type: EOF}
	}
}

// skipWhitespace skips any whitespace that is not a newline character '\n'
func (l *Lexer) skipWhitespace() {
	for l.pos < len(l.input) && l.input[l.pos] != '\n' && unicode.IsSpace(rune(l.input[l.pos])) {
		l.pos++
	}
}

func (l *Lexer) skipAllWhitespace() {
	for l.pos < len(l.input) && unicode.IsSpace(rune(l.input[l.pos])) {
		l.pos++
	}
}

func (l *Lexer) readIdentifier() string {
	start := l.pos
	for l.pos < len(l.input) && (unicode.IsLetter(rune(l.input[l.pos])) || unicode.IsDigit(rune(l.input[l.pos]))) {
		l.pos++
	}
	return l.input[start:l.pos]
}

// readNumber reads a number from the input and returns it as a string. It parses both integers and floats.
// - readNumber supports a leading - sign
// - readNumber supports a leading 0
// - readNumber supports a decimal point
func (l *Lexer) readNumber() Token {
	typ := IntLiteral
	start := l.pos
	l.readDigits()
	if l.pos < len(l.input) && l.input[l.pos] == '.' {
		typ = FloatLiteral
		l.pos++
		l.readDigits()
	}
	return Token{Type: typ, Value: l.input[start:l.pos]}
}

func (l *Lexer) readDigits() {
	for l.pos < len(l.input) && unicode.IsDigit(rune(l.input[l.pos])) {
		l.pos++
	}
}

func (l *Lexer) readString(quote byte) string {
	l.pos++ // skip the opening quote
	start := l.pos
	for l.pos < len(l.input) && l.input[l.pos] != quote {
		l.pos++
	}
	if l.pos >= len(l.input) {
		l.error(fmt.Errorf("unterminated string"))
		return ""
	}
	value := l.input[start:l.pos]
	l.pos++ // skip the closing quote
	return value
}
