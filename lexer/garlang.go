// Code generated by re2c 2.2 on Fri Jun 30 23:50:38 2023, DO NOT EDIT.
package lexer

import (
    "bytes"
    "github.com/masp/garlang/token"
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
	case 'c':
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
	case 'f':
		goto yy57
	case 'i':
		goto yy58
	case 'm':
		goto yy59
	case 'r':
		goto yy60
	case 't':
		goto yy61
	case '{':
		goto yy62
	case '}':
		goto yy64
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
		goto yy66
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
		goto yy68
	}
yy31:
	{ tok = token.Period; lit = "."; return }
yy32:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == '*') {
		goto yy71
	}
	if (yych == '/') {
		goto yy73
	}
	{ tok = token.Slash; lit = "/"; return }
yy34:
	yyaccept = 0
	l.cursor += 1
	l.marker = l.cursor
	yych = l.input[l.cursor]
	if (yych <= '9') {
		if (yych == '.') {
			goto yy68
		}
		if (yych >= '0') {
			goto yy76
		}
	} else {
		if (yych <= 'E') {
			if (yych >= 'E') {
				goto yy79
			}
		} else {
			if (yych == 'e') {
				goto yy79
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
			goto yy68
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
			goto yy79
		} else {
			if (yych == 'e') {
				goto yy79
			}
			goto yy35
		}
	}
yy38:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == '=') {
		goto yy80
	}
	{ tok = token.Colon; lit = ":"; return }
yy40:
	l.cursor += 1
	{ tok = token.Semicolon; lit = ";"; return }
yy42:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == '=') {
		goto yy82
	}
	{ tok = token.Less; lit = "<"; return }
yy44:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == '=') {
		goto yy84
	}
	{ tok = token.Equal; lit = "="; return }
yy46:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == '=') {
		goto yy86
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
	if (yych == 'u') {
		goto yy88
	}
	goto yy49
yy58:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'm') {
		goto yy89
	}
	goto yy49
yy59:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'a') {
		goto yy90
	}
	if (yych == 'o') {
		goto yy91
	}
	goto yy49
yy60:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'e') {
		goto yy92
	}
	goto yy49
yy61:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'u') {
		goto yy93
	}
	if (yych == 'y') {
		goto yy94
	}
	goto yy49
yy62:
	l.cursor += 1
	{ tok = token.LCurlyBracket; lit = "{"; return }
yy64:
	l.cursor += 1
	{ tok = token.RCurlyBracket; lit = "}"; return }
yy66:
	l.cursor += 1
	{ tok = token.BangEqual; lit = "!="; return }
yy68:
	yyaccept = 1
	l.cursor += 1
	l.marker = l.cursor
	yych = l.input[l.cursor]
	if (yych <= 'D') {
		if (yych <= '/') {
			goto yy70
		}
		if (yych <= '9') {
			goto yy68
		}
	} else {
		if (yych <= 'E') {
			goto yy79
		}
		if (yych == 'e') {
			goto yy79
		}
	}
yy70:
	{ tok = token.Float; lit = l.literal(); return }
yy71:
	yyaccept = 2
	l.cursor += 1
	l.marker = l.cursor
	yych = l.input[l.cursor]
	if (yych >= 0x01) {
		goto yy96
	}
yy72:
	{ return l.lexMultiComment() }
yy73:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= '\n') {
		if (yych <= 0x00) {
			goto yy75
		}
		if (yych <= '\t') {
			goto yy73
		}
	} else {
		if (yych != '\r') {
			goto yy73
		}
	}
yy75:
	{ tok = token.Comment; lit = l.literal(); return }
yy76:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= '9') {
		if (yych == '.') {
			goto yy68
		}
		if (yych >= '0') {
			goto yy76
		}
	} else {
		if (yych <= 'E') {
			if (yych >= 'E') {
				goto yy79
			}
		} else {
			if (yych == 'e') {
				goto yy79
			}
		}
	}
yy78:
	l.cursor = l.marker
	if (yyaccept <= 1) {
		if (yyaccept == 0) {
			goto yy35
		} else {
			goto yy70
		}
	} else {
		goto yy72
	}
yy79:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= ',') {
		if (yych == '+') {
			goto yy98
		}
		goto yy78
	} else {
		if (yych <= '-') {
			goto yy98
		}
		if (yych <= '/') {
			goto yy78
		}
		if (yych <= '9') {
			goto yy99
		}
		goto yy78
	}
yy80:
	l.cursor += 1
	{ tok = token.ColonEqual; lit = ":="; return }
yy82:
	l.cursor += 1
	{ tok = token.LessEqual; lit = "<="; return }
yy84:
	l.cursor += 1
	{ tok = token.EqualEqual; lit = "=="; return }
yy86:
	l.cursor += 1
	{ tok = token.GreaterEqual; lit = ">="; return }
yy88:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'n') {
		goto yy101
	}
	goto yy49
yy89:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'p') {
		goto yy102
	}
	goto yy49
yy90:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'p') {
		goto yy103
	}
	goto yy49
yy91:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'd') {
		goto yy105
	}
	goto yy49
yy92:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 't') {
		goto yy106
	}
	goto yy49
yy93:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'p') {
		goto yy107
	}
	goto yy49
yy94:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'p') {
		goto yy108
	}
	goto yy49
yy95:
	l.cursor += 1
	yych = l.input[l.cursor]
yy96:
	if (yych <= 0x00) {
		goto yy78
	}
	if (yych != '*') {
		goto yy95
	}
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == '/') {
		goto yy109
	}
	goto yy95
yy98:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= '/') {
		goto yy78
	}
	if (yych >= ':') {
		goto yy78
	}
yy99:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= '/') {
		goto yy70
	}
	if (yych <= '9') {
		goto yy99
	}
	goto yy70
yy101:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'c') {
		goto yy111
	}
	goto yy49
yy102:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'o') {
		goto yy113
	}
	goto yy49
yy103:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'Z') {
		if (yych <= '/') {
			goto yy104
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
				goto yy104
			}
			if (yych <= 'z') {
				goto yy48
			}
		}
	}
yy104:
	{ tok = token.Map; lit = "map"; return }
yy105:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'u') {
		goto yy114
	}
	goto yy49
yy106:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'u') {
		goto yy115
	}
	goto yy49
yy107:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'l') {
		goto yy116
	}
	goto yy49
yy108:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'e') {
		goto yy117
	}
	goto yy49
yy109:
	l.cursor += 1
	{ tok = token.Comment; lit = l.literal(); return }
yy111:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'Z') {
		if (yych <= '/') {
			goto yy112
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
				goto yy112
			}
			if (yych <= 'z') {
				goto yy48
			}
		}
	}
yy112:
	{ tok = token.Func; lit = "func"; return }
yy113:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'r') {
		goto yy119
	}
	goto yy49
yy114:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'l') {
		goto yy120
	}
	goto yy49
yy115:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'r') {
		goto yy121
	}
	goto yy49
yy116:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'e') {
		goto yy122
	}
	goto yy49
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
	{ tok = token.TypeKeyword; lit = "type"; return }
yy119:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 't') {
		goto yy124
	}
	goto yy49
yy120:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'e') {
		goto yy126
	}
	goto yy49
yy121:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych == 'n') {
		goto yy128
	}
	goto yy49
yy122:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'Z') {
		if (yych <= '/') {
			goto yy123
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
				goto yy123
			}
			if (yych <= 'z') {
				goto yy48
			}
		}
	}
yy123:
	{ tok = token.Tuple; lit = "tuple"; return }
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
	{ tok = token.Import; lit = "import"; return }
yy126:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'Z') {
		if (yych <= '/') {
			goto yy127
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
				goto yy127
			}
			if (yych <= 'z') {
				goto yy48
			}
		}
	}
yy127:
	{ tok = token.Module; lit = "module"; return }
yy128:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'Z') {
		if (yych <= '/') {
			goto yy129
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
				goto yy129
			}
			if (yych <= 'z') {
				goto yy48
			}
		}
	}
yy129:
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
			goto yy132
		}
		if (yych <= '\t') {
			goto yy134
		}
		goto yy136
	} else {
		if (yych == '\\') {
			goto yy138
		}
		goto yy134
	}
yy132:
	l.cursor += 1
	{
			err = ErrUnterminatedString
			tok = token.EOF
            pos = l.file.Pos(l.token)
			return
		}
yy134:
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
yy136:
	l.cursor += 1
yy137:
	{ err = ErrInvalidString; return }
yy138:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych <= 'b') {
		if (yych <= '>') {
			if (yych <= '"') {
				if (yych <= '!') {
					goto yy137
				}
			} else {
				if (yych == '\'') {
					goto yy141
				}
				goto yy137
			}
		} else {
			if (yych <= '\\') {
				if (yych <= '?') {
					goto yy143
				}
				if (yych <= '[') {
					goto yy137
				}
				goto yy145
			} else {
				if (yych <= '`') {
					goto yy137
				}
				if (yych <= 'a') {
					goto yy147
				}
				goto yy149
			}
		}
	} else {
		if (yych <= 'q') {
			if (yych <= 'f') {
				if (yych <= 'e') {
					goto yy137
				}
				goto yy151
			} else {
				if (yych == 'n') {
					goto yy153
				}
				goto yy137
			}
		} else {
			if (yych <= 't') {
				if (yych <= 'r') {
					goto yy155
				}
				if (yych <= 's') {
					goto yy137
				}
				goto yy157
			} else {
				if (yych == 'v') {
					goto yy159
				}
				goto yy137
			}
		}
	}
	l.cursor += 1
	{ buf.WriteByte('"'); continue }
yy141:
	l.cursor += 1
	{ buf.WriteByte('\''); continue }
yy143:
	l.cursor += 1
	{ buf.WriteByte('?'); continue }
yy145:
	l.cursor += 1
	{ buf.WriteByte('\\'); continue }
yy147:
	l.cursor += 1
	{ buf.WriteByte('\a'); continue }
yy149:
	l.cursor += 1
	{ buf.WriteByte('\b'); continue }
yy151:
	l.cursor += 1
	{ buf.WriteByte('\f'); continue }
yy153:
	l.cursor += 1
	{ buf.WriteByte('\n'); continue }
yy155:
	l.cursor += 1
	{ buf.WriteByte('\r'); continue }
yy157:
	l.cursor += 1
	{ buf.WriteByte('\t'); continue }
yy159:
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
		goto yy165
	}
	l.cursor += 1
	{
			err = ErrUnterminatedString
			tok = token.EOF
            pos = l.file.Pos(l.token)
			return
		}
yy165:
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
		goto yy169
	}
	if (yych == '*') {
		goto yy173
	}
	goto yy171
yy169:
	l.cursor += 1
	{
			err = ErrUnterminatedComment
			tok = token.EOF
            pos = l.file.Pos(l.token)
			return
		}
yy171:
	l.cursor += 1
yy172:
	{ continue }
yy173:
	l.cursor += 1
	yych = l.input[l.cursor]
	if (yych != '/') {
		goto yy172
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