package lexer

//go:generate re2go garlang.re -o garlang.go -i

import (
	"errors"

	"github.com/masp/garlang/token"
)

var (
	ErrUnrecognizedToken   = errors.New("unrecognized token")
	ErrInvalidString       = errors.New("invalid string")
	ErrUnterminatedString  = errors.New("unterminated string")
	ErrUnterminatedComment = errors.New("unterminated multiline comment")
)

type TokenType int

type Token struct {
	Pos  token.Pos
	Type token.Type
	Lit  string
}

func (t Token) String() string {
	if t.Lit != "" {
		return t.Lit
	} else {
		return t.Type.String()
	}
}

type Lexer struct {
	file      *token.File
	input     []byte
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
	if l.prevToken.Type.IsLiteral() {
		return true
	}

	switch l.prevToken.Type {
	case token.Identifier, token.RParen, token.RCurlyBracket,
		token.RSquareBracket, token.Return:
		return true
	}
	return false
}

func Lex(input []byte) ([]Token, error) {
	lex := NewLexer("<string>", input)
	tokens := lex.All()
	if lex.HasErrors() {
		return tokens, lex.Errors()
	}
	return tokens, nil
}

func NewLexer(filename string, input []byte) *Lexer {
	if len(input) == 0 || input[len(input)-1] != '\x00' {
		// termination char, faster copying than branching every time in the lexer
		input = append(input, '\x00')
	}
	file := token.NewFile(filename, len(input))
	return &Lexer{file: file, input: input}
}

func (l *Lexer) File() *token.File {
	return l.file
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

func (l *Lexer) NextToken() (tok Token) {
	pos, typ, lit, err := l.lex()
	if err != nil {
		l.error(pos, err)
	}

	tok.Pos = pos
	tok.Lit = lit
	tok.Type = typ
	l.prevToken = tok
	return
}
