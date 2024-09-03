package token

type TokenType = string

type Token struct {
	Literal string
	Type    TokenType
}

// Defining possible token types
const (
	ILLEGAL = "ILLEGAL"
	EOF     = "EOF"

	// Identifiers + literals
	IDENT  = "IDENT"
	CHAR   = "CHAR"
	INT    = "INT"
	FLOAT  = "FLOAT"
	STRING = "STRING"

	// Operators
	ASSIGN   = "="
	PLUS     = "+"
	MINUS    = "-"
	BANG     = "!"
	ASTERISK = "*"
	SLASH    = "/"
	PERCENT  = "%"
	OP       = "OP"

	LT    = "<"
	GT    = ">"
	LT_EQ = "<="
	GT_EQ = ">="

	EQ     = "=="
	NOT_EQ = "!="

	RIGHT_ARROW = "->"

	// Delimiters
	COMMA       = ","
	COLON       = ":"
	COLON_COLON = "::"
	SEMICOLON   = ";"
	DOT         = "."
	BACKTICK    = "`"

	LPAREN   = "("
	RPAREN   = ")"
	LBRACE   = "{"
	RBRACE   = "}"
	LBRACKET = "["
	RBRACKET = "]"

	// Keywords
	TRUE     = "TRUE"
	FALSE    = "FALSE"
	IF       = "IF"
	ELSE     = "ELSE"
	RETURN   = "RETURN"
	USING    = "USING"
	IMPORT   = "IMPORT"
	AS       = "AS"
	FROM     = "FROM"
	WHEN     = "WHEN"
	INFIX    = "INFIX"
	TYPE     = "TYPE"
	BREAK    = "BREAK"
	CONTINUE = "CONTINUE"
	STRUCT   = "STRUCT"
	MAP      = "MAP"
)

var keywords = map[string]TokenType{
	"true":     TRUE,
	"false":    FALSE,
	"if":       IF,
	"else":     ELSE,
	"return":   RETURN,
	"using":    USING,
	"import":   IMPORT,
	"as":       AS,
	"from":     FROM,
	"when":     WHEN,
	"infix":    INFIX,
	"type":     TYPE,
	"break":    BREAK,
	"continue": CONTINUE,
	"struct":   STRUCT,
	"map":      MAP,
}

var DefinedOperators = map[string]TokenType{
	"=": ASSIGN,
	"-": MINUS,
	"!": BANG,
}

func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}
