package token

import "strconv"

type Type int

const (
	Invalid Type = iota

	Comment

	// Literlas/Terminal types
	literal_begin
	Atom
	Identifier
	String
	Integer
	Float
	literal_end

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
	Export
	Return
	Module

	EOF Type = 999 // must be at end
)

var types = [...]string{
	Invalid:      "Invalid",
	Comment:      "Comment",
	Atom:         "Atom",
	Identifier:   "Identifier",
	String:       "String",
	Integer:      "IntLiteral",
	Float:        "FloatLiteral",
	Bang:         "Bang",
	EqualEqual:   "EqualEqual",
	BangEqual:    "BangEqual",
	LessEqual:    "LessEqual",
	Less:         "Less",
	GreaterEqual: "GreaterEqual",
	Greater:      "Greater",
	Plus:         "Plus",
	Minus:        "Minus",
	Slash:        "Slash",
	Star:         "Star",
	Period:       "Period",
	Equal:        "Equal",
	ColonEqual:   "ColonEqual",
	Semicolon:    "Semicolon",
	LeftParen:    "LeftParen",
	RightParen:   "RightParen",
	LeftBrace:    "LeftBrace",
	RightBrace:   "RightBrace",
	Comma:        "Comma",
	Func:         "Func",
	Export:       "Export",
	Return:       "Return",
	Module:       "Module",
	EOF:          "EOF",
}

func (tok Type) String() string {
	s := ""
	if 0 <= tok && tok < Type(len(types)) {
		s = types[tok]
	}
	if s == "" {
		s = "Token(" + strconv.Itoa(int(tok)) + ")"
	}
	return s
}

func (tok Type) IsLiteral() bool {
	return literal_begin < tok && tok < literal_end
}
