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

/*!re2c
		re2c:yyfill:enable = 0;
		re2c:flags:nested-ifs = 1;
		re2c:define:YYCTYPE = byte;
		re2c:define:YYPEEK = "l.input[l.cursor]";
		re2c:define:YYSKIP = "l.cursor += 1";
		re2c:define:YYBACKUP = "l.marker = l.cursor";
		re2c:define:YYRESTORE = "l.cursor = l.marker";

		end = [\x00];
		end { tok = token.EOF; return }
		* { err = ErrUnrecognizedToken; return }

		// Whitespace and new lines
		eol = ("\r\n" | "\n");
		eol {
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

        // Skip whitespace
		[ \t]+ {
			continue
		}

		// Comments
		"//" [^\r\n\x00]* { tok = token.Comment; lit = l.literal(); return }

		// Keywords
		"return" { tok = token.Return; lit = "return"; return }
		"module" { tok = token.Module; lit = "module"; return }
        "func" { tok = token.Func; lit = "func"; return }
        "export" { tok = token.Export; lit = "export"; return }

		// Operators and punctuation
		"(" { tok = token.LeftParen; lit = "("; return }
		")" { tok = token.RightParen; lit = ")"; return }
		"{" { tok = token.LeftBrace; lit = "{"; return }
		"}" { tok = token.RightBrace; lit = "}"; return }
		":=" { tok = token.ColonEqual; lit = ":="; return }
		"=" { tok = token.Equal; lit = "="; return }
        "==" { tok = token.EqualEqual; lit = "=="; return }
        "!=" { tok = token.BangEqual; lit = "!="; return }
        ">=" { tok = token.GreaterEqual; lit = ">="; return }
        "<=" { tok = token.LessEqual; lit = "<="; return }
        ">" { tok = token.Greater; lit = ">"; return }
        "<" { tok = token.Less; lit = "<"; return }
        "+" { tok = token.Plus; lit = "+"; return }
        "-" { tok = token.Minus; lit = "-"; return }
        "*" { tok = token.Star; lit = "*"; return }
        "/" { tok = token.Slash; lit = "/"; return }

		"." { tok = token.Period; lit = "."; return }
		"," { tok = token.Comma; lit = ","; return }
		";" { tok = token.Semicolon; lit = ";"; return }

		// Integer literals
		dec = [1-9][0-9]*;
		dec { tok = token.Integer; lit = l.literal(); return }

		// Floating point numbers
		// from excellent https://re2c.org/examples/c/real_world/example_cxx98.html
		frc = [0-9]* "." [0-9]+ | [0-9]+ ".";
		exp = 'e' [+-]? [0-9]+;
		flt = (frc exp? | [0-9]+ exp);
		flt { tok = token.Float; lit = l.literal(); return }

		// Strings
		["] { return l.lexString('"') }
        ['] {
            pos, tok, lit, err = l.lexString('\'')
            if tok == token.String {
                tok = token.Atom
            }
            return
        }
		[`] { return l.lexRawString('`') }

		// Identifiers
		id = [a-zA-Z_][a-zA-Z_0-9]*;
		id { tok = token.Identifier; lit = l.literal(); return }
*/
    }
}

func (l *Lexer) lexString(quote byte) (pos token.Pos, tok token.Type, lit string, err error) {
	var buf bytes.Buffer
	for {
		var u byte
/*!re2c
		re2c:yyfill:enable = 0;
		re2c:flags:nested-ifs = 1;
		re2c:define:YYCTYPE = byte;
		re2c:define:YYPEEK = "l.input[l.cursor]";
		re2c:define:YYSKIP = "l.cursor += 1";

		* { err = ErrInvalidString; return }
		[\x00] {
			err = ErrUnterminatedString
			tok = token.EOF
            pos = l.file.Pos(l.token)
			return
		}
		[^\n\\]              {
			u = yych
			if u == quote {
				tok = token.String
				pos = l.file.Pos(l.token)
				lit = string(buf.Bytes())
				return
			}
			buf.WriteByte(u)
			continue
		}
		"\\a"                { buf.WriteByte('\a'); continue }
		"\\b"                { buf.WriteByte('\b'); continue }
		"\\f"                { buf.WriteByte('\f'); continue }
		"\\n"                { buf.WriteByte('\n'); continue }
		"\\r"                { buf.WriteByte('\r'); continue }
		"\\t"                { buf.WriteByte('\t'); continue }
		"\\v"                { buf.WriteByte('\v'); continue }
		"\\\\"               { buf.WriteByte('\\'); continue }
		"\\'"                { buf.WriteByte('\''); continue }
		"\\\""               { buf.WriteByte('"'); continue }
		"\\?"                { buf.WriteByte('?'); continue }
*/		
	}
}

func (l *Lexer) lexRawString(quote byte) (pos token.Pos, tok token.Type, lit string, err error) {
	for {
/*!re2c
		re2c:yyfill:enable = 0;
		re2c:flags:nested-ifs = 1;
		re2c:define:YYCTYPE = byte;
		re2c:define:YYPEEK = "l.input[l.cursor]";
		re2c:define:YYSKIP = "l.cursor += 1";

		[\x00] {
			err = ErrUnterminatedString
			tok = token.EOF
            pos = l.file.Pos(l.token)
			return
		}
		[^\x00] {
			if yych == quote {
				tok = token.String
				pos = l.file.Pos(l.token)
				lit = string(l.input[l.token+1:l.cursor-1])
				return
			}
			continue
		}
*/		
	}
}