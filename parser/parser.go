package parser

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"mora/ast"
	"mora/lexer"
	"mora/token"
)

const (
	_           int = iota
	LOWEST          = -1
	ASSIGN          = 0  // =
	EQUALS          = 4  // ==
	LESSGREATER     = 4  // > or <
	SUM             = 6  // +
	PRODUCT         = 7  // *
	PREFIX          = 9  // -X or !X
	CALL            = 10 // myFunction(X)
)

// TODO: attach precedence to environment
var precedence = map[token.TokenType]int{
	token.ASSIGN:   ASSIGN,
	token.EQ:       EQUALS,
	token.NOT_EQ:   EQUALS,
	token.LT:       LESSGREATER,
	token.LT_EQ:    LESSGREATER,
	token.GT:       LESSGREATER,
	token.GT_EQ:    LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.SLASH:    PRODUCT,
	token.ASTERISK: PRODUCT,
	token.PERCENT:  PRODUCT,
	token.LPAREN:   CALL,
	token.LBRACE:   CALL,
	token.LBRACKET: CALL,
	token.DOT:      CALL,
}

type Parser struct {
	l      *lexer.Lexer
	Env    *ast.Environment
	errors []string

	curToken  token.Token
	peekToken token.Token

	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
}

type (
	prefixParseFn = func() ast.Expression
	infixParseFn  = func(ast.Expression) ast.Expression
)

func New(l *lexer.Lexer, env *ast.Environment) *Parser {
	p := &Parser{
		l:      l,
		Env:    env,
		errors: []string{},
	}

	p.nextToken()
	p.nextToken()

	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.registerPrefix(token.IDENT, p.parseIdentifier)
	p.registerPrefix(token.CHAR, p.parseCharLiteral)
	p.registerPrefix(token.TRUE, p.parseBoolean)
	p.registerPrefix(token.FALSE, p.parseBoolean)
	p.registerPrefix(token.INT, p.parseIntegerLiteral)
	p.registerPrefix(token.STRING, p.parseStringLiteral)
	p.registerPrefix(token.MINUS, p.parsePrefixExpression)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)
	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(token.OP, p.parseIdentifier)
	p.registerPrefix(token.STRUCT, p.parseTypeIdentifierExpression)
	p.registerPrefix(token.MAP, p.parseTypeIdentifierExpression)
	p.registerPrefix(token.LBRACKET, p.parseTypeIdentifierExpression)

	p.infixParseFns = make(map[token.TokenType]infixParseFn)
	p.registerInfix(token.PLUS, p.parseInfixExpression)
	p.registerInfix(token.MINUS, p.parseInfixExpression)
	p.registerInfix(token.ASTERISK, p.parseInfixExpression)
	p.registerInfix(token.SLASH, p.parseInfixExpression)
	p.registerInfix(token.NOT_EQ, p.parseInfixExpression)
	p.registerInfix(token.EQ, p.parseInfixExpression)
	p.registerInfix(token.GT, p.parseInfixExpression)
	p.registerInfix(token.GT_EQ, p.parseInfixExpression)
	p.registerInfix(token.LT, p.parseInfixExpression)
	p.registerInfix(token.LT_EQ, p.parseInfixExpression)
	p.registerInfix(token.PERCENT, p.parseInfixExpression)
	p.registerInfix(token.OP, p.parseInfixExpression)
	p.registerInfix(token.LPAREN, p.parseCallExpression)
	p.registerInfix(token.ASSIGN, p.parseDefinition)
	p.registerInfix(token.LBRACE, p.parseBracedLiteral)
	p.registerInfix(token.DOT, p.parseFieldExpression)
	p.registerInfix(token.LBRACKET, p.parseIndexExpression)

	return p
}

func (p *Parser) registerPrefix(t token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[t] = fn
}

func (p *Parser) registerInfix(t token.TokenType, fn infixParseFn) {
	p.infixParseFns[t] = fn
}

func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for !p.curTokenIs(token.EOF) {
		stmt := p.parseStatement(true)
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}

	program.Environment = p.Env

	return program
}

func (p *Parser) parseStatement(requireSemicolon bool) ast.Statement {
	switch p.curToken.Type {
	case token.IDENT, token.OP:
		return p.parseIdentStatement(requireSemicolon)
	case token.IF:
		return p.parseIfStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	case token.BREAK:
		return p.parseBreakStatement()
	case token.CONTINUE:
		return p.parseContinueStatement()
	case token.INFIX:
		return p.parsePrecedenceStatement()
	case token.TYPE:
		return p.parseTypeDefinitionStatement()
	case token.LBRACE:
		return p.parseBlockStatement()
	case token.IMPORT:
		return p.parseImportStatement()
	case token.FROM:
		return p.parseFromStatement()
	case token.USING:
		return p.parseUsingStatement()
	default:
		return p.parseExpressionStatement(requireSemicolon)
	}
}

func (p *Parser) parseIdentStatement(requireSemicolon bool) ast.Statement {
	switch p.peekToken.Type {
	case token.COLON:
		return p.parseIdentDefinition()
	case token.ASSIGN:
		return p.parseIdentAssign()
	default:
		return p.parseExpressionStatement(requireSemicolon)
	}
}

// x := <expression>;
// x :: <expression>;
// x: <type> = <expression>;
// x: <type> : <expression>;
func (p *Parser) parseIdentDefinition() ast.Statement {

	var (
		stmt ast.Statement
		val  ast.Expression
	)

	tok := p.curToken
	name := &ast.Identifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	isOperator := tok.Type == token.OP
	if _, ok := precedence[tok.Literal]; isOperator && !ok {
		precedence[tok.Literal] = 7
	}

	p.nextToken() // :

	if !p.peekTokenIs(token.ASSIGN) {
		p.nextToken()
		name.Type = p.parseTypeIdentifier()
	}

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
		stmt = &ast.IdentDeclaration{
			Token: tok,
			Name:  name,
		}
		return stmt
	}

	if !p.expectPeek(token.ASSIGN, token.SEMICOLON) {
		return nil
	}
	p.nextToken()

	val = p.parseExpression(LOWEST)
	stmt = &ast.IdentDefinition{
		Token: tok,
		Name:  name,
		Value: val,
	}

	if !p.expectPeek(token.SEMICOLON) {
		return nil
	}

	return stmt
}

// x = <expression>;
func (p *Parser) parseIdentAssign() *ast.IdentAssign {
	name := &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
	stmt := &ast.IdentAssign{Token: p.curToken, Name: name}

	p.nextToken() // =
	p.nextToken()

	stmt.Value = p.parseExpression(LOWEST)

	if !p.expectPeek(token.SEMICOLON) {
		return nil
	}

	return stmt
}

func (p *Parser) parseFunctionTypeArguments() ([]ast.TypeIdentifier, bool) {
	typeIdentifiers := []ast.TypeIdentifier{}

	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return typeIdentifiers, true
	}

	if !p.expectPeek(token.IDENT) {
		return nil, false
	}

	typeIdent := p.parseTypeIdentifier()
	typeIdentifiers = append(typeIdentifiers, typeIdent)

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		if !p.expectPeek(token.IDENT) {
			return nil, false
		}
		typeIdent := p.parseTypeIdentifier()
		typeIdentifiers = append(typeIdentifiers, typeIdent)
	}

	if !p.expectPeek(token.RPAREN) {
		return nil, false
	}

	return typeIdentifiers, true
}

func (p *Parser) parseExpressionList(sentinal string) []ast.Expression {
	exprs := []ast.Expression{}

	if p.peekTokenIs(sentinal) {
		p.nextToken()
		return exprs
	}

	p.nextToken()

	exprs = append(exprs, p.parseExpression(LOWEST))

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		exprs = append(exprs, p.parseExpression(LOWEST))
	}
	if !p.expectPeek(sentinal) {
		return nil
	}
	return exprs
}

// <expression>;
func (p *Parser) parseExpressionStatement(requireSemicolon bool) *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.curToken}

	stmt.Value = p.parseExpression(LOWEST)

	if requireSemicolon && !p.expectPeek(token.SEMICOLON) {
		return nil
	}

	return stmt
}

// return <expression> when <expression>;
func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.curToken}

	if !p.peekTokenIs(token.SEMICOLON) && !p.peekTokenIs(token.WHEN) {
		p.nextToken()
		stmt.Value = p.parseExpression(LOWEST)
	}

	stmt.Condition = p.peekParseWhenExpression()

	if !p.expectPeek(token.SEMICOLON) {
		return nil
	}

	return stmt
}

func (p *Parser) parseBreakStatement() *ast.BreakStatement {
	stmt := &ast.BreakStatement{Token: p.curToken}

	stmt.Condition = p.peekParseWhenExpression()

	if !p.expectPeek(token.SEMICOLON) {
		return nil
	}

	return stmt
}

func (p *Parser) parseContinueStatement() *ast.ContinueStatement {
	stmt := &ast.ContinueStatement{Token: p.curToken}

	stmt.Condition = p.peekParseWhenExpression()

	if !p.expectPeek(token.SEMICOLON) {
		return nil
	}

	return stmt
}

func (p *Parser) peekParseWhenExpression() ast.Expression {
	if p.peekTokenIs(token.WHEN) {
		p.nextToken()
		p.nextToken()
		return p.parseExpression(LOWEST)
	}
	return nil
}

func (p *Parser) parsePrecedenceStatement() *ast.PrecedenceStatement {
	stmt := &ast.PrecedenceStatement{Token: p.curToken}

	if !p.expectPeek(token.INT) {
		return nil
	}

	val := p.parseIntegerLiteral().(*ast.IntegerLiteral)
	if val.Value >= 0 &&
		val.Value <= 10 {
		stmt.PrecedenceValue = val
	} else {
		p.precedenceValError(int(val.Value))
		return nil
	}

	if !p.expectPeek(token.OP) {
		return nil
	}
	op := p.curToken.Literal
	stmt.OperatorLiteral = op

	precedence[op] = int(stmt.PrecedenceValue.Value)

	if !p.expectPeek(token.SEMICOLON) {
		return nil
	}

	return stmt
}

func (p *Parser) parseTypeDefinitionStatement() *ast.TypeDefinitionStatement {
	stmt := &ast.TypeDefinitionStatement{Token: p.curToken}

	if !p.expectPeek(token.IDENT) {
		return nil
	}
	stmt.NewType = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	p.nextToken()

	stmt.AliasType = p.parseSingleTypeIdentifier()

	if !p.expectPeek(token.SEMICOLON) {
		return nil
	}

	p.Env.RegisterType(stmt.NewType.Value, stmt.AliasType)

	// token.TypeLiterals[stmt.NewType.Value] = true
	return stmt
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.curToken}
	block.Statements = []ast.Statement{}

	p.nextToken()

	for !p.curTokenIs(token.RBRACE) && !p.curTokenIs(token.EOF) {
		stmt := p.parseStatement(true)
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		p.nextToken()
	}

	return block
}

func (p *Parser) parseImportStatement() ast.Statement {
	is := &ast.ImportStatement{Token: p.curToken}

	if !p.expectPeek(token.IDENT) {
		return nil
	}

	is.Module = p.parseModule()

	if p.peekTokenIs(token.AS) {
		p.nextToken()
		if !p.expectPeek(token.IDENT) {
			return nil
		}
		is.Alias = p.curToken.Literal
	}

	if !p.expectPeek(token.SEMICOLON) {
		return nil
	}

	return is
}

func (p *Parser) parseFromStatement() ast.Statement {
	fs := &ast.FromStatement{Token: p.curToken}

	if !p.expectPeek(token.IDENT) {
		return nil
	}

	fs.Module = p.parseModule()

	if !p.expectPeek(token.IMPORT) {
		return nil
	}

	if !p.expectPeek(token.IDENT, token.OP) {
		return nil
	}

	if p.curToken.Literal == "*" {
		fs.GetAll = true
		if !p.expectPeek(token.SEMICOLON) {
			return nil
		}
		return fs
	}

	fs.Identifiers = append(fs.Identifiers, p.curToken.Literal)

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		if !p.expectPeek(token.IDENT, token.OP) {
			return nil
		}
		fs.Identifiers = append(fs.Identifiers, p.curToken.Literal)
	}

	if !p.expectPeek(token.SEMICOLON) {
		return nil
	}

	return fs
}

func (p *Parser) parseModule() *ast.Module {
	m := &ast.Module{}

	for p.peekTokenIs(token.DOT) {
		m.Directories = append(m.Directories, p.curToken.Literal)
		p.nextToken()
		if !p.expectPeek(token.IDENT) {
			return nil
		}
	}
	m.Name = p.curToken.Literal

	moduleFileName := m.Name + ".mora"
	moduleRelDir := filepath.Join(append(m.Directories, moduleFileName)...)

	// first check if module is in stdlib
	execPath, err := os.Executable()
	if err != nil {
		p.errors = append(p.errors, "unable to import modules")
		return nil
	}
	execDir := filepath.Dir(execPath)
	stdlibDir := filepath.Join(execDir, "stdlib")

	moduleDir := filepath.Join(stdlibDir, moduleRelDir)
	_, err = os.Stat(moduleDir)
	if os.IsNotExist(err) {

		// check if module is in working directory
		cwd, err := os.Getwd()
		if err != nil {
			p.errors = append(p.errors, "unable to get working directory")
			return nil
		}
		moduleDir = filepath.Join(cwd, moduleRelDir)
		_, err = os.Stat(moduleDir)
		if err != nil {
			p.errors = append(p.errors, fmt.Sprintf("unable to find module at %s", moduleDir))
			return nil
		}
	}

	moduleContent, err := os.ReadFile(moduleDir)
	if err != nil {
		p.errors = append(p.errors, fmt.Sprintf("unable to read module at %s", moduleDir))
		return nil
	}

	moduleEnvironment := ast.NewEnvironment()
	moduleLexer := lexer.New(string(moduleContent))
	moduleParser := New(moduleLexer, moduleEnvironment)

	m.Program = moduleParser.ParseProgram()
	p.errors = append(p.errors, moduleParser.errors...)

	return m
}

func (p *Parser) parseUsingStatement() ast.Statement {
	us := &ast.UsingStatement{Token: p.curToken}

	p.nextToken()

	us.FieldedExpression = p.parseExpression(LOWEST)

	if !p.expectPeek(token.SEMICOLON) {
		return nil
	}

	return us
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix()

	for !p.peekTokenIs(token.EOF) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}

		p.nextToken()

		leftExp = infix(leftExp)
	}
	return leftExp
}

func (p *Parser) parsePrefixExpression() ast.Expression {
	if p.curToken.Type == token.MINUS {
		if p.peekTokenIs(token.LPAREN) {
			minusToken := p.curToken
			minusIdent := &ast.Identifier{
				Token: minusToken,
				Value: minusToken.Literal,
			}

			p.nextToken()
			callExp := p.parseCallExpression(minusIdent).(*ast.CallExpression)

			if _, ok := callExp.Function.(*ast.CallExpression); !ok {
				return &ast.PrefixExpression{
					Token:    minusToken,
					Operator: minusToken.Literal,
					Right:    callExp.Argument,
				}
			} else {
				return callExp
			}
		}

		_, ok := p.prefixParseFns[p.peekToken.Type]
		if !ok {
			p.curToken.Type = token.OP
			return p.parseExpression(LOWEST)
		}
	}
	exp := &ast.PrefixExpression{Token: p.curToken, Operator: p.curToken.Literal}
	p.nextToken()
	exp.Right = p.parseExpression(PREFIX)
	return exp
}

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	exp := &ast.CallExpression{
		Token:    p.curToken,
		Function: &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal},
		Argument: left,
	}

	if p.peekTokenIs(token.RPAREN) || p.peekTokenIs(token.SEMICOLON) {
		return exp
	}

	precedence := p.curPrecedence()
	p.nextToken()
	// exp.Arguments = append(exp.Arguments, p.parseExpression(precedence))

	return &ast.CallExpression{
		Token:    p.curToken,
		Function: exp,
		Argument: p.parseExpression(precedence),
	}
}

func (p *Parser) parseCallExpression(left ast.Expression) ast.Expression {
	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return left
	}

	exp := &ast.CallExpression{Token: p.curToken, Function: left}

	p.nextToken()

	exp.Argument = p.parseExpression(LOWEST)

	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return exp
	}

	if !p.expectPeek(token.COMMA) {
		return nil
	}

	return p.parseCallExpression(exp)
}

func (p *Parser) parseDefinition(left ast.Expression) ast.Expression {
	switch left := left.(type) {
	case *ast.CallExpression:
		return p.parseFunctionDefinition(left)
	case *ast.FieldExpression:
		return p.parseFieldDefinition(left)
	case *ast.IndexExpression:
		return p.parseIndexDefinition(left)
	}
	p.invalidLHSError()
	return nil
}

func (p *Parser) parseFunctionDefinition(left *ast.CallExpression) ast.Expression {
	exp := &ast.FunctionDefinition{
		Token: p.curToken,
		Call:  left,
	}

	p.nextToken() // =

	exp.Body = p.parseStatement(false)

	return exp
}

func (p *Parser) parseFieldDefinition(left *ast.FieldExpression) ast.Expression {
	res := &ast.FieldDefinition{Token: p.curToken, Field: left}
	p.nextToken()
	res.Value = p.parseExpression(LOWEST)
	return res
}

func (p *Parser) parseIndexDefinition(left *ast.IndexExpression) ast.Expression {
	res := &ast.IndexDefinition{Token: p.curToken, IndexExpression: left}
	p.nextToken()
	res.Value = p.parseExpression(LOWEST)
	return res
}

func (p *Parser) parseBracedLiteral(left ast.Expression) ast.Expression {
	var (
		te ast.TypeIdentifier
		ok bool
	)

	switch left := left.(type) {
	case *ast.TypeIdentifierExpression:
		te = left.Value
	case *ast.Identifier:
		te, ok = p.Env.GetType(left.Value)
		if !ok {
			p.errors = append(p.errors, fmt.Sprintf("undefined type: %s", left.Value))
			return nil
		}
	default:
		p.errors = append(p.errors, "invalid braced expression")
		return nil
	}

	switch te := te.(type) {
	case *ast.TypeStruct:
		return p.parseStructLiteral(te)
	case *ast.TypeList:
		return p.parseListLiteral(te)
	case *ast.TypeMap:
		return p.parseMapLiteral(te)
	default:
		p.errors = append(p.errors, "unable to parse braced literal")
		return nil
	}
}

func (p *Parser) parseStructLiteral(typeStruct *ast.TypeStruct) ast.Expression {
	sl := &ast.StructLiteral{
		Token:  p.curToken,
		Struct: typeStruct,
		Fields: make(map[string]ast.Expression),
	}

	if p.peekTokenIs(token.RBRACE) {
		p.nextToken()
		return sl
	}

	if !p.expectPeek(token.IDENT) {
		return nil
	}
	fieldName := p.curToken.Literal
	if !p.expectPeek(token.COLON) {
		return nil
	}
	p.nextToken()
	sl.Fields[fieldName] = p.parseExpression(LOWEST)

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		if !p.expectPeek(token.IDENT) {
			return nil
		}
		fieldName := p.curToken.Literal
		if !p.expectPeek(token.COLON) {
			return nil
		}
		p.nextToken()
		sl.Fields[fieldName] = p.parseExpression(LOWEST)
	}

	if !p.expectPeek(token.RBRACE) {
		return nil
	}

	return sl
}

func (p *Parser) parseListLiteral(typeList *ast.TypeList) ast.Expression {
	ll := &ast.ListLiteral{Token: p.curToken, List: typeList}

	if p.peekTokenIs(token.RBRACE) {
		p.nextToken()
		return ll
	}

	p.nextToken()
	ll.Elements = append(ll.Elements, p.parseExpression(LOWEST))

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		ll.Elements = append(ll.Elements, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(token.RBRACE) {
		return nil
	}

	return ll
}

func (p *Parser) parseMapLiteral(typeMap *ast.TypeMap) ast.Expression {
	ml := &ast.MapLiteral{Token: p.curToken, Map: typeMap}

	if p.peekTokenIs(token.RBRACE) {
		p.nextToken()
		return ml
	}

	var keyExp, valExp ast.Expression

	p.nextToken()
	keyExp = p.parseExpression(LOWEST)
	if !p.expectPeek(token.COLON) {
		return nil
	}
	p.nextToken()
	valExp = p.parseExpression(LOWEST)
	ml.Keys = append(ml.Keys, keyExp)
	ml.Values = append(ml.Values, valExp)

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		keyExp = p.parseExpression(LOWEST)
		if !p.expectPeek(token.COLON) {
			return nil
		}
		p.nextToken()
		valExp = p.parseExpression(LOWEST)
		ml.Keys = append(ml.Keys, keyExp)
		ml.Values = append(ml.Values, valExp)
	}

	if !p.expectPeek(token.RBRACE) {
		return nil
	}

	return ml
}

func (p *Parser) parseFieldExpression(left ast.Expression) ast.Expression {
	exp := &ast.FieldExpression{Token: p.curToken, Left: left}
	if !p.expectPeek(token.IDENT, token.OP) {
		return nil
	}
	exp.Field = p.curToken.Literal
	return exp
}

func (p *Parser) parseIndexExpression(left ast.Expression) ast.Expression {
	exp := &ast.IndexExpression{Token: p.curToken, Left: left}

	p.nextToken()
	exp.Index = p.parseExpression(LOWEST)

	if !p.expectPeek(token.RBRACKET) {
		return nil
	}

	return exp
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()
	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(token.RPAREN) {
		return nil
	}
	return exp
}

func (p *Parser) parseBlockExpression() ast.Expression {
	block := &ast.BlockExpression{Token: p.curToken}
	block.Statements = []ast.Statement{}

	p.nextToken()

	for !p.curTokenIs(token.RBRACE) && !p.curTokenIs(token.EOF) {
		stmt := p.parseStatement(false)
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		p.nextToken()
	}

	_, ok := block.Statements[len(block.Statements)-1].(*ast.ExpressionStatement)

	if !ok {
		p.blockExpressionError()
		return nil
	}

	return block
}

func (p *Parser) parseIfStatement() ast.Statement {
	exp := &ast.IfStatement{Token: p.curToken}
	exp.Conditions = make([]ast.Expression, 1)
	exp.Consequences = make([]*ast.BlockStatement, 1)

	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	p.nextToken()

	exp.Conditions[0] = p.parseExpression(LOWEST)

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	exp.Consequences[0] = p.parseBlockStatement()

	// TODO: else if

	if !p.peekTokenIs(token.ELSE) {
		return exp
	}

	p.nextToken()

	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	exp.Alternative = p.parseBlockStatement()

	return exp
}

func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseTypeIdentifierExpression() ast.Expression {
	return &ast.TypeIdentifierExpression{
		Token: p.curToken,
		Value: p.parseTypeIdentifier(),
	}
}

func (p *Parser) parseTypeIdentifier() ast.TypeIdentifier {
	head := p.parseSingleTypeIdentifier()
	if !p.peekTokenIs(token.RIGHT_ARROW) {
		fti, ok := head.(*ast.FunctionTypeIdentifier)
		if ok {
			return fti.Head
		}
		return head
	}

	node := head
	var nextNode ast.TypeIdentifier
	for p.peekTokenIs(token.RIGHT_ARROW) {
		p.nextToken()
		p.nextToken()
		nextNode = node.AppendNode(p.parseSingleTypeIdentifier())
		if !p.peekTokenIs(token.RIGHT_ARROW) {
			fti, ok := nextNode.(*ast.FunctionTypeIdentifier)
			if ok {
				node.AppendNode(fti.Head)
			}
			break
		}
		node = nextNode
	}

	return head
}

func (p *Parser) parseSingleTypeIdentifier() ast.TypeIdentifier {
	switch p.curToken.Type {
	case token.IDENT:
		return p.parseIdentTypeIdentifier()
	case token.LPAREN:
		return p.parseFunctionTypeIdentifier()
	case token.LBRACKET:
		return p.parseListTypeIdentifier()
	case token.STRUCT:
		return p.parseStructTypeIdentifier()
	case token.MAP:
		return p.parseMapTypeIdentifier()
	default:
		p.peekError(token.IDENT, token.LPAREN, token.STRUCT)
		return nil
	}
}

func (p *Parser) parseIdentTypeIdentifier() ast.TypeIdentifier {
	typeTokenLiteral := p.curToken.Literal

	moduleEnvironment, ok := p.Env.GetModule(typeTokenLiteral)

	if !ok {
		ti, ok := ast.PrimitiveTypes[typeTokenLiteral]
		if ok {
			return ti.DeepCopy()
		}

		ti, ok = p.Env.GetType(typeTokenLiteral)
		if ok {
			return ti.DeepCopy()
		}

		p.errors = append(p.errors, fmt.Sprintf("type %s not found", typeTokenLiteral))
		return nil
	}

	if !p.expectPeek(token.DOT) {
		p.errors = append(p.errors, "invalid type")
		return nil
	}
	p.nextToken()
	field := p.curToken.Literal
	ti, ok := moduleEnvironment.GetType(field)
	if !ok {
		p.errors = append(p.errors, fmt.Sprintf("undefined type: %s", field))
		return nil
	}
	return ti.DeepCopy()
}

func (p *Parser) parseFunctionTypeIdentifier() ast.TypeIdentifier {
	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return ast.UNIT
	}
	fnType := &ast.FunctionTypeIdentifier{}

	p.nextToken()
	fnType.Head = p.parseTypeIdentifier()

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	return fnType
}

func (p *Parser) parseListTypeIdentifier() ast.TypeIdentifier {
	res := &ast.TypeList{}

	if !p.expectPeek(token.RBRACKET) {
		return nil
	}

	p.nextToken()

	res.EntryType = p.parseSingleTypeIdentifier()
	return res
}

func (p *Parser) parseStructTypeIdentifier() ast.TypeIdentifier {
	res := &ast.TypeStruct{Fields: make(map[string]ast.TypeIdentifier)}

	if !p.expectPeek(token.LBRACE) {
		return nil
	}
	p.nextToken()

	var (
		field     string
		fieldType ast.TypeIdentifier
	)

	field = p.parseIdentifier().(*ast.Identifier).Value
	if !p.expectPeek(token.COLON) {
		return nil
	}
	p.nextToken()
	fieldType = p.parseTypeIdentifier()
	res.Fields[field] = fieldType

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		field = p.parseIdentifier().(*ast.Identifier).Value
		if !p.expectPeek(token.COLON) {
			return nil
		}
		p.nextToken()
		fieldType = p.parseTypeIdentifier()
		res.Fields[field] = fieldType
	}

	if !p.expectPeek(token.RBRACE) {
		return nil
	}

	return res
}

func (p *Parser) parseMapTypeIdentifier() ast.TypeIdentifier {
	res := &ast.TypeMap{}

	if !p.expectPeek(token.LBRACKET) {
		return nil
	}

	p.nextToken()

	res.KeyType = p.parseSingleTypeIdentifier()

	if !p.expectPeek(token.RBRACKET) {
		return nil
	}
	p.nextToken()

	res.ValueType = p.parseSingleTypeIdentifier()

	return res
}

func (p *Parser) parseCharLiteral() ast.Expression {
	return &ast.CharLiteral{Token: p.curToken, Value: p.curToken.Literal[0]}
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: p.curToken}

	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}

	lit.Value = value

	return lit
}

func (p *Parser) parseBoolean() ast.Expression {
	return &ast.BoolLiteral{Token: p.curToken, Value: p.curTokenIs(token.TRUE)}
}

func (p *Parser) parseStringLiteral() ast.Expression {
	return &ast.StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) expectPeek(ts ...token.TokenType) bool {
	for _, t := range ts {
		if p.peekTokenIs(t) {
			p.nextToken()
			return true
		}
	}
	p.peekError(ts...)
	return false
}

func (p *Parser) peekError(ts ...token.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead",
		strings.Join(ts, " or "),
		p.peekToken.Type,
	)
	p.errors = append(p.errors, msg)
}

func (p *Parser) curPrecedence() int {
	if p, ok := precedence[p.curToken.Literal]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) peekPrecedence() int {
	if p, ok := precedence[p.peekToken.Literal]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) noPrefixParseFnError(t token.TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found", t)
	p.errors = append(p.errors, msg)
}

func (p *Parser) precedenceValError(errVal int) {
	msg := fmt.Sprintf("precedence value out of bounds. got %d, expected >= 0 and <= 10", errVal)
	p.errors = append(p.errors, msg)
}

func (p *Parser) operatorPrecedenceOverwriteError(errOp string) {
	msg := fmt.Sprintf("operator %s already has precedence, cannot be changed", errOp)
	p.errors = append(p.errors, msg)
}

func (p *Parser) blockExpressionError() {
	msg := fmt.Sprintf("block expression must end with expression")
	p.errors = append(p.errors, msg)
}

func (p *Parser) invalidLHSError() {
	msg := fmt.Sprintf("invalid LHS")
	p.errors = append(p.errors, msg)
}
