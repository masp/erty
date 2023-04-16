package lexer

import (
	"errors"

	"github.com/masp/garlang/token"
)

var (
	ErrUnrecognizedToken  = errors.New("unrecognized token")
	ErrInvalidString      = errors.New("invalid string")
	ErrUnterminatedString = errors.New("unterminated string")
)

type TokenType int

type Token struct {
	Pos   token.Pos
	Type  token.Type
	Value string
}

type Lexer struct {
	file      token.File
	input     string
	cursor    int // internal use by lexer
	marker    int // internal use by lexer for backtracking
	token     int // marks the start of the currently scanned token
	prevToken Token

	errors token.ErrorList
}

func (l *Lexer) error(pos token.Pos, err error) {
	l.errors.Add(l.file.Position(pos), err)
}

func (l *Lexer) Errors() token.ErrorList {
	l.errors.RemoveMultiples()
	return l.errors
}

func (l *Lexer) HasErrors() bool {
	return l.errors.Len() > 0
}

// insertSemi returns if one of the following exists:
// an identifier
// an integer, floating-point, imaginary, rune, or string literal
// one of the keywords break, continue, fallthrough, or return
// one of the operators and delimiters ++, --, ), ], or }
func (l *Lexer) insertSemi() bool {
	switch l.prevToken.Type {
	case token.Identifier, token.String, token.RightParen, token.RightBrace, token.Integer, token.Float:
		return true
	}
	return false
}

func Lex(input string) []Token {
	return NewLexer(input).All()
}

func NewLexer(input string) *Lexer {
	if len(input) == 0 || input[len(input)-1] != '\x00' {
		// termination char, faster copying than branching every time in the lexer
		input += "\x00"
	}
	file := token.NewFile("<string>", len(input))
	return &Lexer{file: file, input: input}
}

func (l *Lexer) All() []Token {
	var tokens []Token
	for {
		tok := l.NextToken()
		if tok.Type == token.EOF {
			break
		}
		tokens = append(tokens, tok)
	}
	return tokens
}

func (l *Lexer) literal() string          { return string(l.input[l.token:l.cursor]) }
func (l *Lexer) pos() token.Pos           { return l.file.Pos(l.cursor) }
func (l *Lexer) position() token.Position { return l.file.Position(l.pos()) }

//go:generate re2go lexer.re -o lexer.go -i

func (l *Lexer) NextToken() (tok Token) {
	pos, typ, lit, err := l.lex()
	if err != nil {
		l.error(pos, err)
	}
	tok.Pos = pos
	tok.Value = lit
	tok.Type = typ
	l.prevToken = tok
	return
}
