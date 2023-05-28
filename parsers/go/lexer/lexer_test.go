package lexer

import (
	"strings"
	"testing"
)

type expected = struct {
	tok Token
	lit string
}

func TestTokens(t *testing.T) {
	testToken(t, "Select", expected{tok: SELECT, lit: "SELECT"})
	testToken(t, "from", expected{tok: FROM, lit: "FROM"})
	testToken(t, " select", expected{tok: WS, lit: " "})
	testToken(t, "    whatever", expected{tok: WS, lit: "    "})
	testToken(t, "WhatEver", expected{tok: IDENT, lit: "WhatEver"})
	testToken(t, "*whatever", expected{tok: STAR, lit: "*"})
	testToken(t, "", expected{tok: EOF, lit: ""})
	testToken(t, ",hey", expected{tok: COMMA, lit: ","})
}

func testToken(t *testing.T, input string, exp expected) {
	l := NewLexer(strings.NewReader(input))
	tok, lit := l.Lex()
	if tok != exp.tok {
		t.Errorf("Expected token %d, got %d\n", exp.tok, tok)
	}
	if lit != exp.lit {
		t.Errorf("Expected lit %s, got %s\n", exp.lit, lit)
	}
}
