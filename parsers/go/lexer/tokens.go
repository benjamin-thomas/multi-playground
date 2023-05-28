package lexer

type Token int

//goland:noinspection GoCommentStart
const (
	// Special tokens
	ILLEGAL Token = iota
	EOF
	WS

	// Literals
	IDENT // fields, table_name

	// Misc chars
	STAR
	COMMA

	// Keywords
	SELECT
	FROM
)
