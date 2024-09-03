package ast

import (
	"bytes"
	"fmt"
	"strings"
)

type Object struct {
	Value ObjectValue
	Type  TypeIdentifier
}

func NewObjectByType(t TypeIdentifier) *Object {
	return &Object{Type: t}
}

type ObjectValue interface {
	Inspect() string
}

var (
	// TRUE  = &Boolean{Value: true}
	// FALSE = &Boolean{Value: false}

	ZERO = &Integer{Value: 0}
	ONE  = &Integer{Value: 1}

	NULL_CHAR = &Char{Value: 0}
)

type Boolean struct {
	Value bool
}

func (b *Boolean) Inspect() string { return fmt.Sprintf("%t", b.Value) }

func NativeBooleanObject(value bool) *Integer {
	if value {
		return ONE
	}
	return ZERO
}

type Char struct {
	Value byte
}

func (c *Char) Inspect() string { return fmt.Sprintf("'%c'", c.Value) }

type Integer struct {
	Value int64
}

func (i *Integer) Inspect() string { return fmt.Sprintf("%d", i.Value) }

type Float struct {
	Value float64
}

func (f *Float) Inspect() string { return fmt.Sprintf("%f", f.Value) }

type String struct {
	Value string
}

func (s *String) Inspect() string {
	return fmt.Sprintf("\"%s\"", s.Value)
}

type PatternNode struct {
	Env   *Environment
	Value *Object
}

func NewPatternNode(value *Object, env *Environment) *PatternNode {
	return &PatternNode{Value: value, Env: env}
}

type FunctionBody struct {
	Env   *Environment
	Value Statement
}

func (fb *FunctionBody) Inspect() string {
	return fb.Value.String()
}

type Function struct {
	Argument     string
	DecisionTree map[string]*PatternNode
}

func NewFunction() *Function {
	return &Function{
		DecisionTree: make(map[string]*PatternNode),
	}
}

func (f *Function) Inspect() string {
	return "function"
}

func (f *Function) PushIdent(value *PatternNode) *Object {
	if v, ok := f.DecisionTree[""]; ok {
		return v.Value
	}
	f.DecisionTree[""] = value
	return value.Value
}

func (f *Function) PushPattern(
	patternExp PrimitiveLiteral,
	value *PatternNode,
) *Object {
	pattern := patternExp.String()
	if v, ok := f.DecisionTree[pattern]; ok {
		return v.Value
	}
	f.DecisionTree[pattern] = value
	return value.Value
}

func (f *Function) Call(arg ObjectValue) (ObjectValue, bool) {
	res, ok := f.CallObj(arg)
	return res.Value, ok
}

func (f *Function) CallObj(arg ObjectValue) (*Object, bool) {
	if arg == nil {
		res, ok := f.DecisionTree[""]
		return res.Value, ok
	}
	res, ok := f.DecisionTree[arg.Inspect()]
	if !ok {
		res, ok = f.DecisionTree[""]
		if !ok {
			return nil, ok
		}
		res.Env.Set(f.Argument, arg)
	}
	return res.Value, ok
}

type Struct struct {
	Name   string
	Fields map[string]ObjectValue
}

func (s *Struct) Inspect() string {
	fields := []string{}

	for k, v := range s.Fields {
		fields = append(fields, fmt.Sprintf("%s: %s", k, v.Inspect()))
	}

	var out bytes.Buffer

	out.WriteString(s.Name)
	out.WriteString("{")
	out.WriteString(strings.Join(fields, ", "))
	out.WriteString("}")

	return out.String()
}

type List struct {
	Name     string
	Elements []ObjectValue
}

func (l *List) Inspect() string {
	if l.Name == "[]char" {
		var out bytes.Buffer

		out.WriteByte('"')
		for _, c := range l.Elements {
			out.WriteByte(c.(*Char).Value)
		}
		out.WriteByte('"')

		return out.String()
	}

	fields := []string{}

	for _, v := range l.Elements {
		fields = append(fields, v.Inspect())
	}

	var out bytes.Buffer

	out.WriteString(l.Name)
	out.WriteString("{")
	out.WriteString(strings.Join(fields, ", "))
	out.WriteString("}")

	return out.String()
}

type ReturnValue struct {
	Value ObjectValue
}

func (rv *ReturnValue) Inspect() string { return rv.Value.Inspect() }

type Map struct {
	// TODO:
	// Keys
	Values []ObjectValue
}

type ObjectError struct {
	Message string
}

func (oe *ObjectError) Inspect() string { return oe.Message }
