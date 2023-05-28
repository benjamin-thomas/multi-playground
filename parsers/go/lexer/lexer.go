package lexer

import (
	"bufio"
	"bytes"
	"io"
	"strings"
)

type Lexer struct {
	r *bufio.Reader
}

func NewLexer(r io.Reader) *Lexer {
	return &Lexer{r: bufio.NewReader(r)}
}

func (l *Lexer) read() rune {
	ch, _, err := l.r.ReadRune()
	if err != nil {
		return eof
	}
	return ch
}

func (l *Lexer) unread() {
	must(l.r.UnreadRune())
}

func (l *Lexer) Lex() (tok Token, lit string) {
	ch := l.read()

	if isWhiteSpace(ch) {
		l.unread()
		return l.lexWhiteSpace()
	} else if isLetter(ch) {
		l.unread()
		return l.lexIdent()
	}

	switch ch {
	case eof:
		return EOF, ""
	case '*':
		return STAR, string(ch)

	case ',':
		return COMMA, string(ch)
	}
	return ILLEGAL, string(ch)
}

func (l *Lexer) lexWhiteSpace() (Token, string) {
	var buf bytes.Buffer
	buf.WriteRune(l.read())

	for {
		if ch := l.read(); ch == eof {
			break
		} else if !isWhiteSpace(ch) {
			l.unread()
			break
		} else {
			must2(buf.WriteRune(ch))
		}
	}
	return WS, buf.String()
}

func (l *Lexer) lexIdent() (Token, string) {
	var buf bytes.Buffer
	buf.WriteRune(l.read())

	for {
		if ch := l.read(); ch == eof {
			break
		} else if !isLetter(ch) && !isDigit(ch) && ch != '_' {
			l.unread()
			break
		} else {
			must2(buf.WriteRune(ch))
		}
	}

	// special identifiers
	str := buf.String()
	strUp := strings.ToUpper(str)
	switch strUp {
	case "SELECT":
		return SELECT, strUp
	case "FROM":
		return FROM, strUp
	}

	// regular identifiers
	return IDENT, str
}
