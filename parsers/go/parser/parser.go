package parser

import (
	"example.com/sql-parser/lexer"
	"fmt"
	"io"
)

type Parser struct {
	l   *lexer.Lexer
	buf struct {
		tok lexer.Token
		lit string
		n   int
	}
}

type SelectStatement struct {
	Fields    []string
	TableName string
}

func newParser(r io.Reader) *Parser {
	return &Parser{l: lexer.NewLexer(r)}
}

func (p *Parser) nextToken() (lexer.Token, string) {
	// If we have a token on the buffer, return it
	if p.buf.n != 0 {
		p.buf.n = 0
		return p.buf.tok, p.buf.lit
	}

	// Otherwise, read the next token
	tok, lit := p.l.Lex()

	// Save it to the buffer in case we backtrack
	p.buf.tok, p.buf.lit = tok, lit

	return tok, lit
}

// Enables replaying the last token
func (p *Parser) stepBack() {
	p.buf.n = 1
}

func (p *Parser) chompWhiteSpace() {
	loop := true
	for loop {
		tok, _ := p.nextToken()
		loop = tok == lexer.WS
	}
	p.stepBack()
}

func (p *Parser) parse() (*SelectStatement, error) {
	p.chompWhiteSpace()
	if tok, lit := p.nextToken(); tok != lexer.SELECT {
		return nil, fmt.Errorf("found %q, expected SELECT", lit)
	}

	stmt := &SelectStatement{}
	for {
		p.chompWhiteSpace()
		tok, lit := p.nextToken()
		if tok != lexer.IDENT && tok != lexer.STAR {
			return nil, fmt.Errorf("found %q, expected field", lit)
		}
		stmt.Fields = append(stmt.Fields, lit)

		// If the next token is not a comma then break the loop
		p.chompWhiteSpace()
		if tok, _ := p.nextToken(); tok != lexer.COMMA {
			p.stepBack()
			break
		}
	}

	// Next, we should see FROM
	p.chompWhiteSpace()
	tok, lit := p.nextToken()
	if tok != lexer.FROM {
		return nil, fmt.Errorf("found %q, expected FROM", lit)
	}

	// Next, we should see the table name
	p.chompWhiteSpace()
	tok, lit = p.nextToken()
	if tok != lexer.IDENT {
		return nil, fmt.Errorf("found %q, expected table name", lit)
	}
	stmt.TableName = lit
	return stmt, nil
}
