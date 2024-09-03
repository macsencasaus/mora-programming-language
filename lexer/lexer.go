package lexer

import (
	"mora/token"
)

type Lexer struct {
	input        string
	position     int
	readPosition int
	ch           byte
}

func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition += 1
}

func (l *Lexer) NextToken() token.Token {
	var tok token.Token
	l.skipWhitespace()

	switch l.ch {
	case ',':
		tok = newToken(token.COMMA, l.ch)
	case ':':
		if l.peekChar() == ':' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.COLON_COLON, Literal: literal}
		} else {
			tok = newToken(token.COLON, l.ch)
		}
	case ';':
		tok = newToken(token.SEMICOLON, l.ch)
	case '.':
		tok = newToken(token.DOT, l.ch)
	case '(':
		tok = newToken(token.LPAREN, l.ch)
	case ')':
		tok = newToken(token.RPAREN, l.ch)
	case '{':
		tok = newToken(token.LBRACE, l.ch)
	case '}':
		tok = newToken(token.RBRACE, l.ch)
	case '[':
		tok = newToken(token.LBRACKET, l.ch)
	case ']':
		tok = newToken(token.RBRACKET, l.ch)
	case '\'':
		tok.Type = token.CHAR
		tok.Literal = l.readCharLiteral()
		if l.ch != '\'' {
			return newToken(token.ILLEGAL, l.ch)
		}
	case '"':
		tok.Type = token.STRING
		tok.Literal = l.readStringLiteral()
	case '`':
		tok.Type = token.OP
		tok = newToken(token.BACKTICK, l.ch)
	case 0:
		tok = token.Token{Type: token.EOF, Literal: ""}
	default:
		switch {
		case isLetter(l.ch):
			tok = l.readIdentifierToken()
			return tok
		case isDigit(l.ch):
			tok = l.readNumberToken()
			return tok
		case isOperator(l.ch):
			tok = l.readOperatorToken()
			if tok.Literal == "//" {
				l.eatLine()
				return l.NextToken()
			}
			return tok
		default:
			tok = newToken(token.ILLEGAL, l.ch)
		}
	}

	l.readChar()
	return tok
}

func (l *Lexer) readIdentifierToken() token.Token {
	position := l.position
	for isValidIdentCh(l.ch) {
		l.readChar()
	}
	literal := l.input[position:l.position]
	tokType := token.LookupIdent(literal)
	return token.Token{Literal: literal, Type: tokType}
}

func (l *Lexer) readNumberToken() token.Token {
	position := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	literal := l.input[position:l.position]
	return token.Token{Literal: literal, Type: token.INT}
}

func (l *Lexer) readOperatorToken() token.Token {
	position := l.position
	for isOperator(l.ch) {
		l.readChar()
	}
	literal := l.input[position:l.position]
	if literal == "->" {
		return token.Token{Literal: literal, Type: token.RIGHT_ARROW}
	}
	tokType, ok := token.DefinedOperators[literal]
	if !ok {
		tokType = token.OP
	}
	return token.Token{Literal: literal, Type: tokType}
}

func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}

func (l *Lexer) readStringLiteral() string {
	position := l.position + 1
	for {
		l.readChar()
		if l.ch == '"' || l.ch == 0 {
			break
		}
	}
	return l.input[position:l.position]
}

func (l *Lexer) readCharLiteral() string {
	l.readChar()
	c := string(l.ch)
	l.readChar()
	return c
}

func newToken(tokenType token.TokenType, ch byte) token.Token {
	return token.Token{Type: tokenType, Literal: string(ch)}
}

func isLetter(ch byte) bool {
	return ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z' || ch == '_'
}

func isValidIdentCh(ch byte) bool {
	return isLetter(ch) || isDigit(ch) || ch == '\''
}

func isDigit(ch byte) bool {
	return ch >= '0' && ch <= '9'
}

func isOperator(ch byte) bool {
	return ch == '=' ||
		ch == '!' ||
		ch == '+' ||
		ch == '-' ||
		ch == '*' ||
		ch == '/' ||
		ch == '>' ||
		ch == '<' ||
		ch == '~' ||
		ch == '$' ||
		ch == '%' ||
		ch == '^' ||
		ch == '&' ||
		ch == '|' ||
		ch == '_' ||
		ch == ':'
}

func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

func (l *Lexer) eatLine() {
	for l.ch != '\n' && l.ch != 0 {
		l.readChar()
	}
	l.readChar()
}
