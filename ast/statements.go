package ast

import (
	"bytes"
	"strings"

	"mora/token"
)

type Statement interface {
	Node
	statementNode()
}

type ExpressionStatement struct {
	Token token.Token
	Value Expression
}

func (es *ExpressionStatement) statementNode()       {}
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Literal }
func (es *ExpressionStatement) String() string       { return es.Value.String() + ";" }

type IdentDefinition struct {
	Token token.Token
	Name  *Identifier
	Value Expression
}

func (id *IdentDefinition) statementNode()       {}
func (id *IdentDefinition) TokenLiteral() string { return id.Token.Literal }
func (id *IdentDefinition) String() string {
	var out bytes.Buffer

	out.WriteString(id.Name.String())
	out.WriteString(" :")
	if id.Name.Type != nil {
		out.WriteString(" ")
		out.WriteString(id.Name.Type.String())
		out.WriteString(" ")
	}
	out.WriteString("= ")
	out.WriteString(id.Value.String())
	out.WriteString(";")

	return out.String()
}

type IdentDeclaration struct {
	Token token.Token
	Name  *Identifier
}

func (id *IdentDeclaration) statementNode()       {}
func (id *IdentDeclaration) TokenLiteral() string { return id.Token.Literal }
func (id *IdentDeclaration) String() string {
	var out bytes.Buffer

	out.WriteString(id.Name.String())
	out.WriteString(": ")
	out.WriteString(id.Name.Type.String())
	out.WriteString(";")

	return out.String()
}

type IdentAssign struct {
	Token token.Token
	Name  *Identifier
	Value Expression
}

func (ias *IdentAssign) statementNode()       {}
func (ias *IdentAssign) TokenLiteral() string { return ias.Token.Literal }
func (ias *IdentAssign) String() string {
	var out bytes.Buffer

	out.WriteString(ias.Name.String())
	out.WriteString(" = ")
	out.WriteString(ias.Value.String())
	out.WriteString(";")

	return out.String()
}

type ReturnStatement struct {
	Token     token.Token
	Value     Expression
	Condition Expression
}

func (rs *ReturnStatement) statementNode()       {}
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }
func (rs *ReturnStatement) String() string {
	var out bytes.Buffer

	out.WriteString("return ")
	if rs.Value != nil {
		out.WriteString(rs.Value.String())
	}
	if rs.Condition != nil {
		out.WriteString(" when ")
		out.WriteString(rs.Condition.String())
	}
	out.WriteString(";")

	return out.String()
}

type BreakStatement struct {
	Token     token.Token
	Condition Expression
}

func (bs *BreakStatement) statementNode()       {}
func (bs *BreakStatement) TokenLiteral() string { return bs.Token.Literal }
func (bs *BreakStatement) String() string {
	var out bytes.Buffer

	out.WriteString("break")

	if bs.Condition != nil {
		out.WriteString(" when ")
		out.WriteString(bs.Condition.String())
	}

	out.WriteString(";")

	return out.String()
}

type ContinueStatement struct {
	Token     token.Token
	Condition Expression
}

func (cs *ContinueStatement) statementNode()       {}
func (cs *ContinueStatement) TokenLiteral() string { return cs.Token.Literal }
func (cs *ContinueStatement) String() string {
	var out bytes.Buffer

	out.WriteString("continue")

	if cs.Condition != nil {
		out.WriteString(" when ")
		out.WriteString(cs.Condition.String())
	}

	out.WriteString(";")

	return out.String()
}

type PrecedenceStatement struct {
	Token           token.Token
	PrecedenceValue *IntegerLiteral
	OperatorLiteral string
}

func (ps *PrecedenceStatement) statementNode()       {}
func (ps *PrecedenceStatement) TokenLiteral() string { return ps.Token.Literal }
func (ps *PrecedenceStatement) String() string {
	var out bytes.Buffer

	out.WriteString("infix ")
	out.WriteString(ps.PrecedenceValue.String())
	out.WriteString(" ")
	out.WriteString(ps.OperatorLiteral)
	out.WriteString(";")

	return out.String()
}

type TypeDefinitionStatement struct {
	Token     token.Token
	NewType   *Identifier
	AliasType TypeIdentifier
}

func (tds *TypeDefinitionStatement) statementNode()       {}
func (tds *TypeDefinitionStatement) TokenLiteral() string { return tds.Token.Literal }
func (tds *TypeDefinitionStatement) String() string {
	var out bytes.Buffer

	out.WriteString("type ")
	out.WriteString(tds.NewType.String())
	out.WriteString(" ")
	out.WriteString(tds.AliasType.String())
	out.WriteString(";")

	return out.String()
}

type BlockStatement struct {
	Token      token.Token
	Statements []Statement
	Env        *Environment
}

func (bs *BlockStatement) statementNode()       {}
func (bs *BlockStatement) TokenLiteral() string { return bs.Token.Literal }
func (bs *BlockStatement) String() string {
	var out bytes.Buffer

	stmts := []string{}
	for _, stmt := range bs.Statements {
		stmts = append(stmts, "    "+stmt.String())
	}

	out.WriteString("{\n")
	out.WriteString(strings.Join(stmts, "\n"))
	out.WriteString("\n}")

	return out.String()
}

type FunctionDefinition struct {
	Token token.Token
	Call  *CallExpression
	Body  Statement
}

func (fd *FunctionDefinition) expressionNode()      {}
func (fd *FunctionDefinition) TokenLiteral() string { return fd.Token.Literal }
func (fd *FunctionDefinition) String() string {
	var out bytes.Buffer

	out.WriteString(fd.Call.String())
	out.WriteString(" = ")
	out.WriteString(fd.Body.String())

	return out.String()
}

type TypeclassDefinition struct {
	Token         token.Token
	TypeclassName *Identifier
	GenericType   TypeIdentifier
	Functions     []*IdentDeclaration
}

func (td *TypeclassDefinition) statementNode()       {}
func (td *TypeclassDefinition) TokenLiteral() string { return td.Token.Literal }
func (td *TypeclassDefinition) String() string {
	var out bytes.Buffer

	out.WriteString("typeclass ")
	out.WriteString(td.TypeclassName.String())
	out.WriteString(" ")
	out.WriteString(td.GenericType.String())
	out.WriteString(" {\n")

	for _, f := range td.Functions {
		out.WriteString("    ")
		out.WriteString(f.String())
		out.WriteString("\n")
	}

	out.WriteString("}\n")

	return out.String()
}

type ImportStatement struct {
	Token  token.Token
	Module *Module
	Alias  string
}

func (is *ImportStatement) statementNode()       {}
func (is *ImportStatement) TokenLiteral() string { return is.Token.Literal }
func (is *ImportStatement) String() string {
	var out bytes.Buffer

	out.WriteString("import ")
	out.WriteString(is.Module.String())
	if is.Alias != "" {
		out.WriteString(" as ")
		out.WriteString(is.Alias)
	}
	out.WriteString(";")

	return out.String()
}

type FromStatement struct {
	Token       token.Token
	Module      *Module
	Identifiers []string
	GetAll      bool
}

func (fs *FromStatement) statementNode()       {}
func (fs *FromStatement) TokenLiteral() string { return fs.Token.Literal }
func (fs *FromStatement) String() string {

	var out bytes.Buffer

	out.WriteString("from ")
	out.WriteString(fs.Module.String())
	out.WriteString(" import ")

	if fs.GetAll {
		out.WriteString("*")
	} else {
		out.WriteString(strings.Join(fs.Identifiers, ", "))
	}
	out.WriteString(";")

	return out.String()
}

type Module struct {
	Directories []string
	Name        string
	Program     *Program
}

func (m *Module) String() string {
	var out bytes.Buffer

	out.WriteString(strings.Join(append(m.Directories, m.Name), "."))

	return out.String()
}

func (m *Module) Inspect() string {
	return m.String()
}

type UsingStatement struct {
	Token             token.Token
	FieldedExpression Expression
}

func (us *UsingStatement) statementNode()       {}
func (us *UsingStatement) TokenLiteral() string { return us.Token.Literal }
func (us *UsingStatement) String() string {
	var out bytes.Buffer

	out.WriteString("using ")
	out.WriteString(us.FieldedExpression.String())
	out.WriteString(";")

	return out.String()
}
