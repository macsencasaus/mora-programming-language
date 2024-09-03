package ast

import (
	"bytes"
	"strings"

	"mora/token"
)

type Expression interface {
	Node
	expressionNode()
}

type Identifier struct {
	Token token.Token
	Value string
	Type  TypeIdentifier
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
func (i *Identifier) String() string       { return i.Value }

type PrimitiveLiteral interface {
	Expression
	TypeLiteral() string
}

type BoolLiteral struct {
	Token token.Token
	Value bool
}

func (bl *BoolLiteral) expressionNode()      {}
func (bl *BoolLiteral) TokenLiteral() string { return bl.Token.Literal }
func (bl *BoolLiteral) String() string       { return bl.Token.Literal }
func (bl *BoolLiteral) TypeLiteral() string  { return PRIM_TYPE_BOOL_LITERAL }

type CharLiteral struct {
	Token token.Token
	Value byte
}

func (cl *CharLiteral) expressionNode()      {}
func (cl *CharLiteral) TokenLiteral() string { return cl.Token.Literal }
func (cl *CharLiteral) String() string {
	var out bytes.Buffer

	out.WriteRune('\'')
	out.WriteByte(cl.Value)
	out.WriteRune('\'')

	return out.String()
}
func (cl *CharLiteral) TypeLiteral() string { return PRIM_TYPE_CHAR_LITERAL }

type IntegerLiteral struct {
	Token token.Token
	Value int64
}

func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) String() string       { return il.Token.Literal }
func (il *IntegerLiteral) TypeLiteral() string  { return PRIM_TYPE_INT_LITERAL }

type FloatLiteral struct {
	Token token.Token
	Value float64
}

func (fl *FloatLiteral) expressionNode()      {}
func (fl *FloatLiteral) TokenLiteral() string { return fl.Token.Literal }
func (fl *FloatLiteral) String() string       { return fl.Token.Literal }
func (fl *FloatLiteral) TypeLiteral() string  { return PRIM_TYPE_FLOAT_LITERAL }

type StringLiteral struct {
	Token token.Token
	Value string
}

func (sl *StringLiteral) expressionNode()      {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }
func (sl *StringLiteral) String() string {
	var out bytes.Buffer

	out.WriteString("\"")
	out.WriteString(sl.Token.Literal)
	out.WriteString("\"")

	return out.String()
}
func (sl *StringLiteral) TypeLiteral() string { return PRIM_TYPE_STRING_LITERAL }

type StructLiteral struct {
	Token  token.Token
	Struct *TypeStruct
	Fields map[string]Expression
}

func (sl *StructLiteral) expressionNode()      {}
func (sl *StructLiteral) TokenLiteral() string { return sl.Token.Literal }
func (sl *StructLiteral) String() string {
	var out bytes.Buffer

	fields := []string{}
	for k, v := range sl.Fields {
		fields = append(fields, k+": "+v.String())
	}

	out.WriteString(sl.Struct.String())
	out.WriteString("{")
	out.WriteString(strings.Join(fields, ", "))
	out.WriteString("}")

	return out.String()
}

type ListLiteral struct {
	Token    token.Token
	List     *TypeList
	Elements []Expression
}

func (ll *ListLiteral) expressionNode()      {}
func (ll *ListLiteral) TokenLiteral() string { return ll.Token.Literal }
func (ll *ListLiteral) String() string {
	elems := []string{}
	for _, e := range ll.Elements {
		elems = append(elems, e.String())
	}

	var out bytes.Buffer

	out.WriteString(ll.List.String())
	out.WriteString("{")
	out.WriteString(strings.Join(elems, ", "))
	out.WriteString("}")

	return out.String()
}

type MapLiteral struct {
	Token  token.Token
	Map    *TypeMap
	Keys   []Expression
	Values []Expression
}

func (ml *MapLiteral) expressionNode()      {}
func (ml *MapLiteral) TokenLiteral() string { return ml.Token.Literal }
func (ml *MapLiteral) String() string {
	elems := []string{}
	for i, k := range ml.Keys {
		elems = append(elems, k.String()+": "+ml.Values[i].String())
	}

	var out bytes.Buffer

	out.WriteString(ml.Map.String())
	out.WriteString("{")
	out.WriteString(strings.Join(elems, ", "))
	out.WriteString("}")

	return out.String()
}

type FieldExpression struct {
	Token token.Token
	Left  Expression
	Field string
}

func (fe *FieldExpression) expressionNode()      {}
func (fe *FieldExpression) TokenLiteral() string { return fe.Token.Literal }
func (fe *FieldExpression) String() string {
	var out bytes.Buffer

	out.WriteString(fe.Left.String())
	out.WriteString(".")
	out.WriteString(fe.Field)

	return out.String()
}

type FieldDefinition struct {
	Token token.Token
	Field *FieldExpression
	Value Expression
}

func (fd *FieldDefinition) expressionNode()      {}
func (fd *FieldDefinition) TokenLiteral() string { return fd.Token.Literal }
func (fd *FieldDefinition) String() string {
	var out bytes.Buffer

	out.WriteString(fd.Field.String())
	out.WriteString(" = ")
	out.WriteString(fd.Value.String())

	return out.String()
}

type IndexDefinition struct {
	Token           token.Token
	IndexExpression *IndexExpression
	Value           Expression
}

func (id *IndexDefinition) expressionNode()      {}
func (id *IndexDefinition) TokenLiteral() string { return id.Token.Literal }
func (id *IndexDefinition) String() string {
	var out bytes.Buffer

	out.WriteString(id.IndexExpression.String())
	out.WriteString(" = ")
	out.WriteString(id.Value.String())

	return out.String()
}

type PrefixExpression struct {
	Token    token.Token
	Operator string
	Right    Expression
}

func (pe *PrefixExpression) expressionNode()      {}
func (pe *PrefixExpression) TokenLiteral() string { return pe.Token.Literal }
func (pe *PrefixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(pe.Operator)
	out.WriteString(pe.Right.String())
	out.WriteString(")")

	return out.String()
}

type BlockExpression struct {
	Token      token.Token
	Statements []Statement
	Type       TypeIdentifier
}

func (be *BlockExpression) expressionNode()      {}
func (be *BlockExpression) TokenLiteral() string { return be.Token.Literal }
func (be *BlockExpression) String() string {
	var out bytes.Buffer

	out.WriteString("{\n")

	for _, e := range be.Statements {
		out.WriteString("  ")
		out.WriteString(e.String())
		out.WriteString("\n")
	}

	out.WriteString("}")

	return out.String()
}

type IfStatement struct {
	Token        token.Token
	Conditions   []Expression
	Consequences []*BlockStatement
	Alternative  *BlockStatement
}

func (ie *IfStatement) statementNode()       {}
func (ie *IfStatement) TokenLiteral() string { return ie.Token.Literal }
func (ie *IfStatement) String() string {
	var out bytes.Buffer

	out.WriteString("if (")
	out.WriteString(ie.Conditions[0].String())
	out.WriteString(") ")
	out.WriteString(ie.Consequences[0].String())

	// TODO: else if loop

	if ie.Alternative != nil {
		out.WriteString(" else ")
		out.WriteString(ie.Alternative.String())
	}

	return out.String()
}

type CallExpression struct {
	Token    token.Token
	Function Expression
	Argument Expression
}

func (fc *CallExpression) expressionNode()      {}
func (fc *CallExpression) TokenLiteral() string { return fc.Token.Literal }
func (fc *CallExpression) String() string {
	var out bytes.Buffer

	out.WriteString(fc.Function.String())
	out.WriteString("(")
	out.WriteString(fc.Argument.String())
	out.WriteString(")")

	return out.String()
}

type IndexExpression struct {
	Token token.Token
	Left  Expression
	Index Expression
}

func (ie *IndexExpression) expressionNode()      {}
func (ie *IndexExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IndexExpression) String() string {
	var out bytes.Buffer

	out.WriteString(ie.Left.String())
	out.WriteString("[")
	out.WriteString(ie.Index.String())
	out.WriteString("]")

	return out.String()
}

type TypeIdentifierExpression struct {
	Token token.Token
	Value TypeIdentifier
}

func (tie *TypeIdentifierExpression) expressionNode()      {}
func (tie *TypeIdentifierExpression) TokenLiteral() string { return tie.Token.Literal }
func (tie *TypeIdentifierExpression) String() string       { return tie.Value.String() }
