// Code generated by re2c 2.2 on Fri Jul 21 19:35:28 2023, DO NOT EDIT.
package lexer

import (
    "bytes"
    "github.com/masp/ertylang/token"
)

func (l *Lexer) lex() (pos token.Pos, tok token.Type, lit string, err error) {
    for {
		lit = ""
		pos = l.pos()
		l.token = l.cursor


{
	var yych byte
	yyaccept := 0
	yych = l.input[l.cursor]
	switch (yych) {
	case 0x00:
		goto yy2
	case '\t':
		fallthrough
	case ' ':
		goto yy6
	case '\n':
		goto yy9
	case '\r':
		goto yy11
	case '!':
		goto yy12
	case '"':
		goto yy14
	case '\'':
		goto yy16
	case '(':
		goto yy18
	case ')':
		goto yy20
	case '*':
		goto yy22
	case '+':
		goto yy24
	case ',':
		goto yy26
	case '-':
		goto yy28
	case '.':
		goto yy30
	case '/':
		goto yy32
	case '0':
		goto yy34
	case '1':
		fallthrough
	case '2':
		fallthrough
	case '3':
		fallthrough
	case '4':
		fallthrough
	case '5':
		fallthrough
	case '6':
		fallthrough
	case '7':
		fallthrough
	case '8':
		fallthrough
	case '9':
		goto yy36
	case ':':
		goto yy38
	case ';':
		goto yy40
	case '<':
		goto yy42
	case '=':
		goto yy44
	case '>':
		goto yy46
	case 'A':
		fallthrough
	case 'B':
		fallthrough
	case 'C':
		fallthrough
	case 'D':
		fallthrough
	case 'E':
		fallthrough
	case 'F':
		fallthrough
	case 'G':
		fallthrough
	case 'H':
		fallthrough
	case 'I':
		fallthrough
	case 'J':
		fallthrough
	case 'K':
		fallthrough
	case 'L':
		fallthrough
	case 'M':
		fallthrough
	case 'N':
		fallthrough
	case 'O':
		fallthrough
	case 'P':
		fallthrough
	case 'Q':
		fallthrough
	case 'R':
		fallthrough
	case 'S':
		fallthrough
	case 'T':
		fallthrough
	case 'U':
		fallthrough
	case 'V':
		fallthrough
	case 'W':
		fallthrough
	case 'X':
		fallthrough
	case 'Y':
		fallthrough
	case 'Z':
		fallthrough
	case '_':
		fallthrough
	case 'a':
		fallthrough
	case 'b':
		fallthrough
	case 'd':
		fallthrough
	case 'e':
		fallthrough
	case 'g':
		fallthrough
	case 'h':
		fallthrough
	case 'j':
		fallthrough
	case 'k':
		fallthrough
	case 'l':
		fallthrough
	case 'n':
		fallthrough
	case 'o':
		fallthrough
	case 'p':
		fallthrough
	case 'q':
		fallthrough
	case 's':
		fallthrough
	case 'u':
		fallthrough
	case 'v':
		fallthrough
	case 'w':
		fallthrough
	case 'x':
		fallthrough
	case 'y':
		fallthrough
	case 'z':
		goto yy48
	case '[':
		goto yy51
	case ']':
		goto yy53
	case '`':
		goto yy55
	case 'c':
		goto yy57
	case 'f':
		goto yy58
	case 'i':
		goto yy59
	case 'm':
		goto yy60
	case 'r':
		goto yy61
	case 't':
		goto yy62
	case '{':
		goto yy63
	case '}':
		goto yy65
	default:
		goto yy4
	}
yy2:
	l.cursor += 1
	{ tok = token.EOF; return }
yy4:
	l.cursor += 1
yy5:
	{ err = ErrUnrecognizedToken; return }
yy6:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == '\t') {
		goto yy6
	}
	if (yych == ' ') {
		goto yy6
	}
	{
			continue
		}
yy9:
	l.cursor += 1
	{
			if l.insertSemi() {
				l.cursor = l.token // Has the effect of "inserting" the semicolon in the input
				tok = token.Semicolon
				lit = "\n"
				return
			} else {
				l.file.AddLine(l.token)
				continue
			}
		}
yy11:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == '\n') {
		goto yy9
	}
	goto yy5
yy12:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == '=') {
		goto yy67
	}
	{ tok = token.Bang; lit = "!"; return }
yy14:
	l.cursor += 1
	{ return l.lexString('"') }
yy16:
	l.cursor += 1
	{
            pos, tok, lit, err = l.lexString('\'')
            if tok == token.String {
                tok = token.Atom
            }
            return
        }
yy18:
	l.cursor += 1
	{ tok = token.LParen; lit = "("; return }
yy20:
	l.cursor += 1
	{ tok = token.RParen; lit = ")"; return }
yy22:
	l.cursor += 1
	{ tok = token.Star; lit = "*"; return }
yy24:
	l.cursor += 1
	{ tok = token.Plus; lit = "+"; return }
yy26:
	l.cursor += 1
	{ tok = token.Comma; lit = ","; return }
yy28:
	l.cursor += 1
	{ tok = token.Minus; lit = "-"; return }
yy30:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= '/') {
		goto yy31
	}
	if (yych <= '9') {
		goto yy69
	}
yy31:
	{ tok = token.Period; lit = "."; return }
yy32:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == '*') {
		goto yy72
	}
	if (yych == '/') {
		goto yy74
	}
	{ tok = token.Slash; lit = "/"; return }
yy34:
	yyaccept = 0
	l.cursor += 1
	l.marker = l.cursor
	yych = l.input[l.cursor]
	if (yych <= '9') {
		if (yych == '.') {
			goto yy69
		}
		if (yych >= '0') {
			goto yy77
		}
	} else {
		if (yych <= 'E') {
			if (yych >= 'E') {
				goto yy80
			}
		} else {
			if (yych == 'e') {
				goto yy80
			}
		}
	}
yy35:
	{ tok = token.Integer; lit = l.literal(); return }
yy36:
	yyaccept = 0
	l.cursor += 1
	l.marker = l.cursor
	yych = l.input[l.cursor]
	if (yych <= '9') {
		if (yych == '.') {
			goto yy69
		}
		if (yych <= '/') {
			goto yy35
		}
		goto yy36
	} else {
		if (yych <= 'E') {
			if (yych <= 'D') {
				goto yy35
			}
			goto yy80
		} else {
			if (yych == 'e') {
				goto yy80
			}
			goto yy35
		}
	}
yy38:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == '=') {
		goto yy81
	}
	{ tok = token.Colon; lit = ":"; return }
yy40:
	l.cursor += 1
	{ tok = token.Semicolon; lit = ";"; return }
yy42:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == '=') {
		goto yy83
	}
	{ tok = token.Less; lit = "<"; return }
yy44:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == '=') {
		goto yy85
	}
	{ tok = token.Equal; lit = "="; return }
yy46:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == '=') {
		goto yy87
	}
	{ tok = token.Greater; lit = ">"; return }
yy48:
	l.cursor += 1
	yych = l.input[l.cursor]
yy49:
	if (yych <= 'Z') {
		if (yych <= '/') {
			goto yy50
		}
		if (yych <= '9') {
			goto yy48
		}
		if (yych >= 'A') {
			goto yy48
		}
	} else {
		if (yych <= '_') {
			if (yych >= '_') {
				goto yy48
			}
		} else {
			if (yych <= '`') {
				goto yy50
			}
			if (yych <= 'z') {
				goto yy48
			}
		}
	}
yy50:
	{ tok = token.Identifier; lit = l.literal(); return }
yy51:
	l.cursor += 1
	{ tok = token.LSquareBracket; lit = "["; return }
yy53:
	l.cursor += 1
	{ tok = token.RSquareBracket; lit = "]"; return }
yy55:
	l.cursor += 1
	{ return l.lexRawString('`') }
yy57:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'a') {
		goto yy89
	}
	goto yy49
yy58:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'u') {
		goto yy90
	}
	goto yy49
yy59:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'm') {
		goto yy91
	}
	goto yy49
yy60:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'a') {
		goto yy92
	}
	if (yych == 'o') {
		goto yy93
	}
	goto yy49
yy61:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'e') {
		goto yy94
	}
	goto yy49
yy62:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'u') {
		goto yy95
	}
	if (yych == 'y') {
		goto yy96
	}
	goto yy49
yy63:
	l.cursor += 1
	{ tok = token.LCurlyBracket; lit = "{"; return }
yy65:
	l.cursor += 1
	{ tok = token.RCurlyBracket; lit = "}"; return }
yy67:
	l.cursor += 1
	{ tok = token.BangEqual; lit = "!="; return }
yy69:
	yyaccept = 1
	l.cursor += 1
	l.marker = l.cursor
	yych = l.input[l.cursor]
	if (yych <= 'D') {
		if (yych <= '/') {
			goto yy71
		}
		if (yych <= '9') {
			goto yy69
		}
	} else {
		if (yych <= 'E') {
			goto yy80
		}
		if (yych == 'e') {
			goto yy80
		}
	}
yy71:
	{ tok = token.Float; lit = l.literal(); return }
yy72:
	yyaccept = 2
	l.cursor += 1
	l.marker = l.cursor
	yych = l.input[l.cursor]
	if (yych >= 0x01) {
		goto yy98
	}
yy73:
	{ return l.lexMultiComment() }
yy74:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= '\n') {
		if (yych <= 0x00) {
			goto yy76
		}
		if (yych <= '\t') {
			goto yy74
		}
	} else {
		if (yych != '\r') {
			goto yy74
		}
	}
yy76:
	{ tok = token.Comment; lit = l.literal(); return }
yy77:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= '9') {
		if (yych == '.') {
			goto yy69
		}
		if (yych >= '0') {
			goto yy77
		}
	} else {
		if (yych <= 'E') {
			if (yych >= 'E') {
				goto yy80
			}
		} else {
			if (yych == 'e') {
				goto yy80
			}
		}
	}
yy79:
	l.cursor = l.marker
	if (yyaccept <= 1) {
		if (yyaccept == 0) {
			goto yy35
		} else {
			goto yy71
		}
	} else {
		goto yy73
	}
yy80:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= ',') {
		if (yych == '+') {
			goto yy100
		}
		goto yy79
	} else {
		if (yych <= '-') {
			goto yy100
		}
		if (yych <= '/') {
			goto yy79
		}
		if (yych <= '9') {
			goto yy101
		}
		goto yy79
	}
yy81:
	l.cursor += 1
	{ tok = token.ColonEqual; lit = ":="; return }
yy83:
	l.cursor += 1
	{ tok = token.LessEqual; lit = "<="; return }
yy85:
	l.cursor += 1
	{ tok = token.EqualEqual; lit = "=="; return }
yy87:
	l.cursor += 1
	{ tok = token.GreaterEqual; lit = ">="; return }
yy89:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 's') {
		goto yy103
	}
	goto yy49
yy90:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'n') {
		goto yy104
	}
	goto yy49
yy91:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'p') {
		goto yy105
	}
	goto yy49
yy92:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'p') {
		goto yy106
	}
	if (yych == 't') {
		goto yy108
	}
	goto yy49
yy93:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'd') {
		goto yy109
	}
	goto yy49
yy94:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 't') {
		goto yy110
	}
	goto yy49
yy95:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'p') {
		goto yy111
	}
	goto yy49
yy96:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'p') {
		goto yy112
	}
	goto yy49
yy97:
	l.cursor += 1
	yych = l.input[l.cursor]
yy98:
	if (yych <= 0x00) {
		goto yy79
	}
	if (yych != '*') {
		goto yy97
	}
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == '/') {
		goto yy113
	}
	goto yy97
yy100:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= '/') {
		goto yy79
	}
	if (yych >= ':') {
		goto yy79
	}
yy101:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= '/') {
		goto yy71
	}
	if (yych <= '9') {
		goto yy101
	}
	goto yy71
yy103:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'e') {
		goto yy115
	}
	goto yy49
yy104:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'c') {
		goto yy117
	}
	goto yy49
yy105:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'o') {
		goto yy119
	}
	goto yy49
yy106:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'Z') {
		if (yych <= '/') {
			goto yy107
		}
		if (yych <= '9') {
			goto yy48
		}
		if (yych >= 'A') {
			goto yy48
		}
	} else {
		if (yych <= '_') {
			if (yych >= '_') {
				goto yy48
			}
		} else {
			if (yych <= '`') {
				goto yy107
			}
			if (yych <= 'z') {
				goto yy48
			}
		}
	}
yy107:
	{ tok = token.Map; lit = "map"; return }
yy108:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'c') {
		goto yy120
	}
	goto yy49
yy109:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'u') {
		goto yy121
	}
	goto yy49
yy110:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'u') {
		goto yy122
	}
	goto yy49
yy111:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'l') {
		goto yy123
	}
	goto yy49
yy112:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'e') {
		goto yy124
	}
	goto yy49
yy113:
	l.cursor += 1
	{ tok = token.Comment; lit = l.literal(); return }
yy115:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'Z') {
		if (yych <= '/') {
			goto yy116
		}
		if (yych <= '9') {
			goto yy48
		}
		if (yych >= 'A') {
			goto yy48
		}
	} else {
		if (yych <= '_') {
			if (yych >= '_') {
				goto yy48
			}
		} else {
			if (yych <= '`') {
				goto yy116
			}
			if (yych <= 'z') {
				goto yy48
			}
		}
	}
yy116:
	{ tok = token.Case; lit = "case"; return }
yy117:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'Z') {
		if (yych <= '/') {
			goto yy118
		}
		if (yych <= '9') {
			goto yy48
		}
		if (yych >= 'A') {
			goto yy48
		}
	} else {
		if (yych <= '_') {
			if (yych >= '_') {
				goto yy48
			}
		} else {
			if (yych <= '`') {
				goto yy118
			}
			if (yych <= 'z') {
				goto yy48
			}
		}
	}
yy118:
	{ tok = token.Func; lit = "func"; return }
yy119:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'r') {
		goto yy126
	}
	goto yy49
yy120:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'h') {
		goto yy127
	}
	goto yy49
yy121:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'l') {
		goto yy129
	}
	goto yy49
yy122:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'r') {
		goto yy130
	}
	goto yy49
yy123:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'e') {
		goto yy131
	}
	goto yy49
yy124:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'Z') {
		if (yych <= '/') {
			goto yy125
		}
		if (yych <= '9') {
			goto yy48
		}
		if (yych >= 'A') {
			goto yy48
		}
	} else {
		if (yych <= '_') {
			if (yych >= '_') {
				goto yy48
			}
		} else {
			if (yych <= '`') {
				goto yy125
			}
			if (yych <= 'z') {
				goto yy48
			}
		}
	}
yy125:
	{ tok = token.TypeKeyword; lit = "type"; return }
yy126:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 't') {
		goto yy133
	}
	goto yy49
yy127:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'Z') {
		if (yych <= '/') {
			goto yy128
		}
		if (yych <= '9') {
			goto yy48
		}
		if (yych >= 'A') {
			goto yy48
		}
	} else {
		if (yych <= '_') {
			if (yych >= '_') {
				goto yy48
			}
		} else {
			if (yych <= '`') {
				goto yy128
			}
			if (yych <= 'z') {
				goto yy48
			}
		}
	}
yy128:
	{ tok = token.Match; lit = "match"; return }
yy129:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'e') {
		goto yy135
	}
	goto yy49
yy130:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'n') {
		goto yy137
	}
	goto yy49
yy131:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'Z') {
		if (yych <= '/') {
			goto yy132
		}
		if (yych <= '9') {
			goto yy48
		}
		if (yych >= 'A') {
			goto yy48
		}
	} else {
		if (yych <= '_') {
			if (yych >= '_') {
				goto yy48
			}
		} else {
			if (yych <= '`') {
				goto yy132
			}
			if (yych <= 'z') {
				goto yy48
			}
		}
	}
yy132:
	{ tok = token.Tuple; lit = "tuple"; return }
yy133:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'Z') {
		if (yych <= '/') {
			goto yy134
		}
		if (yych <= '9') {
			goto yy48
		}
		if (yych >= 'A') {
			goto yy48
		}
	} else {
		if (yych <= '_') {
			if (yych >= '_') {
				goto yy48
			}
		} else {
			if (yych <= '`') {
				goto yy134
			}
			if (yych <= 'z') {
				goto yy48
			}
		}
	}
yy134:
	{ tok = token.Import; lit = "import"; return }
yy135:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'Z') {
		if (yych <= '/') {
			goto yy136
		}
		if (yych <= '9') {
			goto yy48
		}
		if (yych >= 'A') {
			goto yy48
		}
	} else {
		if (yych <= '_') {
			if (yych >= '_') {
				goto yy48
			}
		} else {
			if (yych <= '`') {
				goto yy136
			}
			if (yych <= 'z') {
				goto yy48
			}
		}
	}
yy136:
	{ tok = token.Module; lit = "module"; return }
yy137:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'Z') {
		if (yych <= '/') {
			goto yy138
		}
		if (yych <= '9') {
			goto yy48
		}
		if (yych >= 'A') {
			goto yy48
		}
	} else {
		if (yych <= '_') {
			if (yych >= '_') {
				goto yy48
			}
		} else {
			if (yych <= '`') {
				goto yy138
			}
			if (yych <= 'z') {
				goto yy48
			}
		}
	}
yy138:
	{ tok = token.Return; lit = "return"; return }
}

    }
}

func (l *Lexer) lexString(quote byte) (pos token.Pos, tok token.Type, lit string, err error) {
	var buf bytes.Buffer
	buf.WriteByte(quote)
	for {
		var u byte

{
	var yych byte
	yych = l.input[l.cursor]
	if (yych <= '\n') {
		if (yych <= 0x00) {
			goto yy141
		}
		if (yych <= '\t') {
			goto yy143
		}
		goto yy145
	} else {
		if (yych == '\\') {
			goto yy147
		}
		goto yy143
	}
yy141:
	l.cursor += 1
	{
			err = ErrUnterminatedString
			tok = token.EOF
            pos = l.file.Pos(l.token)
			return
		}
yy143:
	l.cursor += 1
	{
			u = yych
			buf.WriteByte(u)
			if u == quote {
				tok = token.String
				pos = l.file.Pos(l.token)
				lit = string(buf.Bytes())
				return
			}
			continue
		}
yy145:
	l.cursor += 1
yy146:
	{ err = ErrInvalidString; return }
yy147:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'b') {
		if (yych <= '>') {
			if (yych <= '"') {
				if (yych <= '!') {
					goto yy146
				}
			} else {
				if (yych == '\'') {
					goto yy150
				}
				goto yy146
			}
		} else {
			if (yych <= '\\') {
				if (yych <= '?') {
					goto yy152
				}
				if (yych <= '[') {
					goto yy146
				}
				goto yy154
			} else {
				if (yych <= '`') {
					goto yy146
				}
				if (yych <= 'a') {
					goto yy156
				}
				goto yy158
			}
		}
	} else {
		if (yych <= 'q') {
			if (yych <= 'f') {
				if (yych <= 'e') {
					goto yy146
				}
				goto yy160
			} else {
				if (yych == 'n') {
					goto yy162
				}
				goto yy146
			}
		} else {
			if (yych <= 't') {
				if (yych <= 'r') {
					goto yy164
				}
				if (yych <= 's') {
					goto yy146
				}
				goto yy166
			} else {
				if (yych == 'v') {
					goto yy168
				}
				goto yy146
			}
		}
	}
	l.cursor += 1
	{ buf.WriteByte('"'); continue }
yy150:
	l.cursor += 1
	{ buf.WriteByte('\''); continue }
yy152:
	l.cursor += 1
	{ buf.WriteByte('?'); continue }
yy154:
	l.cursor += 1
	{ buf.WriteByte('\\'); continue }
yy156:
	l.cursor += 1
	{ buf.WriteByte('\a'); continue }
yy158:
	l.cursor += 1
	{ buf.WriteByte('\b'); continue }
yy160:
	l.cursor += 1
	{ buf.WriteByte('\f'); continue }
yy162:
	l.cursor += 1
	{ buf.WriteByte('\n'); continue }
yy164:
	l.cursor += 1
	{ buf.WriteByte('\r'); continue }
yy166:
	l.cursor += 1
	{ buf.WriteByte('\t'); continue }
yy168:
	l.cursor += 1
	{ buf.WriteByte('\v'); continue }
}
		
	}
}

func (l *Lexer) lexRawString(quote byte) (pos token.Pos, tok token.Type, lit string, err error) {
	for {

{
	var yych byte
	yych = l.input[l.cursor]
	if (yych >= 0x01) {
		goto yy174
	}
	l.cursor += 1
	{
			err = ErrUnterminatedString
			tok = token.EOF
            pos = l.file.Pos(l.token)
			return
		}
yy174:
	l.cursor += 1
	{
			if yych == quote {
				tok = token.String
				pos = l.file.Pos(l.token)
				lit = string(l.input[l.token:l.cursor])
				return
			}
			continue
		}
}
		
	}
}

func (l *Lexer) lexMultiComment() (pos token.Pos, tok token.Type, lit string, err error) {
	for {

{
	var yych byte
	yych = l.input[l.cursor]
	if (yych <= 0x00) {
		goto yy178
	}
	if (yych == '*') {
		goto yy182
	}
	goto yy180
yy178:
	l.cursor += 1
	{
			err = ErrUnterminatedComment
			tok = token.EOF
            pos = l.file.Pos(l.token)
			return
		}
yy180:
	l.cursor += 1
yy181:
	{ continue }
yy182:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych != '/') {
		goto yy181
	}
	l.cursor += 1
	{
			tok = token.Comment
			pos = l.file.Pos(l.token)
			lit = string(l.input[l.token+2:l.cursor])
			return
		}
}
		
	}
}