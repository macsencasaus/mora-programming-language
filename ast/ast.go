package ast

import (
	"bytes"
)

const (
	TYPE_TYPE_LITERAL        = "type"
	PRIM_TYPE_BOOL_LITERAL   = "bool"
	PRIM_TYPE_CHAR_LITERAL   = "char"
	PRIM_TYPE_INT_LITERAL    = "int"
	PRIM_TYPE_FLOAT_LITERAL  = "float"
	PRIM_TYPE_STRING_LITERAL = "string"
)

var (
	TYPE_BOOL   = TYPE_INT
	TYPE_CHAR   = newPrimitiveType(PRIM_TYPE_CHAR_LITERAL)
	TYPE_INT    = newPrimitiveType(PRIM_TYPE_INT_LITERAL)
	TYPE_FLOAT  = newPrimitiveType(PRIM_TYPE_FLOAT_LITERAL)
	TYPE_STRING = &TypeList{
		Name:      PRIM_TYPE_STRING_LITERAL,
		EntryType: TYPE_CHAR,
	}
)

var PrimitiveTypes = map[string]TypeIdentifier{
	"bool":   TYPE_BOOL,
	"char":   TYPE_CHAR,
	"int":    TYPE_INT,
	"float":  TYPE_FLOAT,
	"string": TYPE_STRING,
}

type Node interface {
	TokenLiteral() string
	String() string
}

type Program struct {
	Environment *Environment
	Statements  []Statement
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	}
	return ""
}

func (p *Program) String() string {
	var out bytes.Buffer

	for _, stmt := range p.Statements {
		out.WriteString(stmt.String())
		out.WriteString("\n")
	}

	return out.String()
}

func newPrimitiveType(t string) *PrimitiveTypeIdentifier {
	return &PrimitiveTypeIdentifier{Value: t}
}
