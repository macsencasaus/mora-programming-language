package semantics

import (
	"fmt"
	"mora/ast"
)

func Check(node ast.Node, expectedReturnType *ast.TypeReturnStatement, env *ast.Environment) ast.TypeIdentifier {
	switch node := node.(type) {

	case *ast.Program:
		return checkProgram(node.Statements, env)

	case ast.PrimitiveLiteral:
		return ast.PrimitiveTypes[node.TypeLiteral()]

	case *ast.ExpressionStatement:
		return Check(node.Value, nil, env)

	case *ast.Identifier:
		return checkIdentifier(node, env)

	case *ast.PrefixExpression:
		right := Check(node.Right, nil, env)
		if IsError(right) {
			return right
		}
		return checkPrefixExpression(node.Operator, right)

	case *ast.CallExpression:
		return checkCallExpression(node, env)

	case *ast.FunctionDefinition:
		callExp := node.Call
		expectedType, res, extendedEnv := extendTypeEnv(
			callExp,
			env,
		)
		if IsError(expectedType) {
			return expectedType
		}

		err := checkFunctionBody(node.Body, expectedType, extendedEnv)

		if IsError(err) {
			return err
		}

		res.Value = &ast.FunctionBody{
			Env: extendedEnv, Value: node.Body,
		}

		return ast.UNIT

	case *ast.IdentDeclaration:
		return checkIdentDeclaration(node, env)

	case *ast.IdentDefinition:
		return checkIdentDefinition(node, env)

	case *ast.IdentAssign:
		return checkIdentAssign(node, env)

	case *ast.BlockStatement:
		var blockType ast.TypeIdentifier
		blockType, node.Env = checkBlockStatement(node.Statements, expectedReturnType, env)
		return blockType

	case *ast.ReturnStatement:
		exprType := Check(node.Value, nil, env)
		if IsError(exprType) {
			return exprType
		}
		if node.Condition != nil {
			condType := Check(node.Condition, nil, env)
			if !condType.Equals(ast.TYPE_BOOL) {
				return typeError(node.Condition, condType, ast.TYPE_BOOL)
			}
			if IsError(condType) {
				return condType
			}
		}
		return &ast.TypeReturnStatement{
			Value: exprType,
			Cond:  node.Condition != nil,
		}

	case *ast.IfStatement:
		conditionsType := checkIfConditions(node.Conditions, env)
		if IsError(conditionsType) {
			return conditionsType
		}

		return checkIfConsequences(node.Consequences, node.Alternative, expectedReturnType, env)

	case *ast.StructLiteral:
		s := node.Struct
		err := checkStructFields(s.Fields, node.Fields, env)
		if IsError(err) {
			return err
		}
		return s

	case *ast.FieldExpression:
		leftType := Check(node.Left, nil, env)
		switch leftType := leftType.(type) {
		case *ast.TypeStruct:
			fieldType, ok := leftType.Fields[node.Field]
			if !ok {
				return newError("field %s undefined in struct %s", node.Field, leftType)
			}
			return fieldType
		case *ast.TypeModule:
			moduleEnvironment, _ := env.GetModule(leftType.Name)
			fieldType, ok := moduleEnvironment.GetObjectType(node.Field)
			if !ok {
				return newError(
					"field %s undefined as identifier in module %s",
					node.Field,
					leftType,
				)
			}
			return fieldType
		default:
			return newError("Invalid LHS for field expression")
		}

	case *ast.FieldDefinition:
		fieldType := Check(node.Field, nil, env)
		if IsError(fieldType) {
			return fieldType
		}
		valueType := Check(node.Value, nil, env)
		if IsError(valueType) {
			return valueType
		}
		if !fieldType.Equals(valueType) {
			return typeError(node.Value, valueType, fieldType)
		}
		return ast.UNIT

	case *ast.ListLiteral:
		l := node.List
		err := checkListElements(
			l.EntryType.DetachAndCopy(),
			node.Elements,
			env,
		)
		if IsError(err) {
			return err
		}
		return node.List

	case *ast.IndexExpression:
		var res ast.TypeIdentifier

		leftType := Check(node.Left, nil, env)
		if IsError(leftType) {
			return leftType
		}

		switch leftType := leftType.(type) {
		case *ast.TypeList:
			indexType := Check(node.Index, nil, env)
			if !indexType.Equals(ast.TYPE_INT) {
				return typeError(node.Index, indexType, ast.TYPE_INT)
			}
			res = leftType.EntryType.DetachAndCopy()
		case *ast.TypeMap:
			indexType := Check(node.Index, nil, env)
			if !indexType.Equals(leftType.KeyType) {
				return typeError(node.Index, indexType, leftType.KeyType)
			}
			res = leftType.ValueType
		default:
			return newError("invalid index expression for type %s", leftType)
		}

		return res

	case *ast.IndexDefinition:
		expectedType := Check(node.IndexExpression, nil, env)
		if IsError(expectedType) {
			return expectedType
		}

		assignedType := Check(node.Value, nil, env)
		if IsError(assignedType) {
			return assignedType
		}

		if !expectedType.Equals(assignedType) {
			return typeError(node.Value, assignedType, expectedType)
		}

		return ast.UNIT

	case *ast.MapLiteral:
		keyType, valType := node.Map.KeyType, node.Map.ValueType
		err := checkMapKeys(keyType.DetachAndCopy(), node.Keys, env)
		if IsError(err) {
			return err
		}

		err = checkListElements(valType.DetachAndCopy(), node.Values, env)
		if IsError(err) {
			return err
		}

		return node.Map

	case *ast.ImportStatement:
		err, moduleEnvironment := checkModule(node.Module)
		if IsError(err) {
			return err
		}
		moduleName := node.Module.Name
		if node.Alias != "" {
			moduleName = node.Alias
		}
		env.SetModule(moduleName, moduleEnvironment)
		env.SetNewObjectByType(moduleName, &ast.TypeModule{Name: moduleName})
		env.Set(moduleName, node.Module)
		return ast.UNIT

	case *ast.FromStatement:
		err, moduleEnvironment := checkModule(node.Module)
		if IsError(err) {
			return err
		}
		env.SetModule(node.Module.Name, moduleEnvironment)

		if node.GetAll {
			for k, v := range moduleEnvironment.GetObjectStore() {
				env.SetNewObjectByType(k, v.Type)
			}
			for k, v := range moduleEnvironment.GetTypeStore() {
				env.RegisterType(k, v)
			}
			return ast.UNIT
		}

		for _, ident := range node.Identifiers {
			obj, ok := moduleEnvironment.GetObject(ident)
			if !ok {
				ti, ok := moduleEnvironment.GetType(ident)
				if !ok {
					return newError("%s undefined in %s", ident, node.Module.Name)
				}
				env.RegisterType(ident, ti)
			}
			env.SetObject(ident, obj)
		}

	case *ast.UsingStatement:
		fe := Check(node.FieldedExpression, nil, env)

		switch fe := fe.(type) {
		case *ast.TypeStruct:
			for k, v := range fe.Fields {
				env.SetNewObjectByType(k, v)
			}
		case *ast.TypeModule:
			moduleEnvironment, _ := env.GetModule(fe.Name)
			for k, v := range moduleEnvironment.GetObjectStore() {
				env.SetNewObjectByType(k, v.Type)
			}
			for k, v := range moduleEnvironment.GetTypeStore() {
				env.RegisterType(k, v)
			}
		default:
			return newError("invalid fielded expression")
		}

	case *ast.TypeIdentifierExpression:
		return node.Value
	}

	return ast.UNIT
}

func checkExpressions(exps []ast.Expression, env *ast.Environment) []ast.TypeIdentifier {
	var result []ast.TypeIdentifier

	for _, e := range exps {
		checked := Check(e, nil, env)
		if IsError(checked) {
			return []ast.TypeIdentifier{checked}
		}
		result = append(result, checked)
	}

	return result
}

func checkProgram(statements []ast.Statement, env *ast.Environment) ast.TypeIdentifier {
	var result ast.TypeIdentifier

	for _, statement := range statements {
		result = Check(statement, nil, env)

		switch result := result.(type) {
		case *ast.TypeCheckerError:
			return result
		case *ast.TypeReturnStatement:
			return newError("unexpected return statement")
		}
	}
	if result == nil {
		return ast.UNIT
	}
	return result
}

func checkIdentifier(ident *ast.Identifier, env *ast.Environment) ast.TypeIdentifier {
	t, ok := env.GetObjectType(ident.Value)

	if !ok {
		return undefinedError(ident.Value)
	} else {
		ident.Type = t
		return t
	}
}

func checkPrefixExpression(operator string, rightType ast.TypeIdentifier) ast.TypeIdentifier {
	switch operator {
	case "-":
		return checkMinusOperator(rightType)
	case "!":
		return checkBangOperator(rightType)
	default:
		return newError("unknown operator %s%s", operator, rightType.String())
	}
}

func checkMinusOperator(rightType ast.TypeIdentifier) ast.TypeIdentifier {
	if rightType.Equals(ast.TYPE_INT) || rightType.Equals(ast.TYPE_FLOAT) {
		return rightType
	} else {
		return newError("unknown operator -%s", rightType.String())
	}
}

func checkBangOperator(rightType ast.TypeIdentifier) ast.TypeIdentifier {
	if rightType.Equals(ast.TYPE_BOOL) {
		return rightType
	} else {
		return newError("unkown operator !%s", rightType.String())
	}
}

func extendTypeEnv(
	callExp *ast.CallExpression,
	env *ast.Environment,
) (ast.TypeIdentifier, *ast.Object, *ast.Environment) {
	function := callExp.Function
	arg := callExp.Argument

	var (
		fnType ast.TypeIdentifier
		fnObj  *ast.Object
		ok     bool
	)

	outerEnv := env

	switch function := function.(type) {
	case *ast.Identifier:
		fnObj, ok = env.GetObject(function.Value)
		if !ok {
			return undefinedError(function.Value), nil, nil
		}
		fnType = fnObj.Type
	case *ast.CallExpression:
		fnType, fnObj, outerEnv = extendTypeEnv(function, env)
	}

	if IsError(fnType) {
		return fnType, nil, nil
	}

	var fnObjVal *ast.Function
	if fnObj.Value != nil {
		fnObjVal = fnObj.Value.(*ast.Function)
	} else {
		fnObjVal = ast.NewFunction()
	}

	expectedType := fnType.Next()
	fnResult := &ast.Object{Type: expectedType}

	extendedEnv := ast.NewEnclosedEnvironment(outerEnv)

	switch arg := arg.(type) {
	case ast.PrimitiveLiteral:
		litType := Check(arg, nil, env)
		if IsError(litType) {
			return litType, nil, nil
		}
		if !litType.Equals(fnType.DetachAndCopy()) {
			return typeError(
				arg,
				litType,
				fnType.DetachAndCopy(),
			), nil, nil
		}
		fnResult = fnObjVal.PushPattern(
			arg,
			ast.NewPatternNode(fnResult, extendedEnv),
		)
	case *ast.Identifier:
		extendedEnv.SetNewObjectByType(arg.Value, fnType.DetachAndCopy())
		fnResult = fnObjVal.PushIdent(ast.NewPatternNode(fnResult, extendedEnv))
		fnObjVal.Argument = arg.Value
	default:
		return newError("invalid argument (%s) for function definition", arg.String()), nil, nil
	}
	fnObj.Value = fnObjVal

	return expectedType, fnResult, extendedEnv
}

func checkCallExpression(callExp *ast.CallExpression, env *ast.Environment) ast.TypeIdentifier {
	fnType := Check(callExp.Function, nil, env)
	if IsError(fnType) {
		return fnType
	}
	argType := Check(callExp.Argument, nil, env)
	if IsError(argType) {
		return argType
	}
	resType := fnType.Reduce(argType)
	if resType == nil {
		// TODO: better error message
		return newError("invalid arguments")
	}
	return resType
}

func checkFunctionBody(functionBody ast.Statement, expectedType ast.TypeIdentifier, env *ast.Environment) ast.TypeIdentifier {

	var result ast.TypeIdentifier

	switch functionBody := functionBody.(type) {
	case *ast.BlockStatement:
		expectedReturnType := &ast.TypeReturnStatement{
			Value: expectedType,
		}
		result, functionBody.Env = checkBlockStatement(
			functionBody.Statements,
			expectedReturnType,
			env,
		)

		if IsError(result) {
			return result
		}

		if !result.Equals(expectedReturnType) {
			return newError("cannot return %s as %s", result, expectedType)
		}
	case *ast.ExpressionStatement:
		result = Check(functionBody.Value, nil, env)

		if IsError(result) {
			return result
		}

		if !expectedType.Equals(result) {
			return typeError(functionBody.Value, result, expectedType)
		}
	}

	return result
}

func getInfixOperatorResult(operator string, leftType, rightType ast.TypeIdentifier, env *ast.Environment) ast.TypeIdentifier {
	operatorType, ok := env.GetObjectType(operator)
	if !ok {
		return newError("unkown operator %s %s %s", leftType.String(), operator, rightType.String())
	}
	resultType := operatorType.Reduce(leftType, rightType)
	if resultType == nil {
		return newError("unkown operator %s %s %s", leftType.String(), operator, rightType.String())
	}
	return resultType

}

func checkIdentDefinition(id *ast.IdentDefinition, env *ast.Environment) ast.TypeIdentifier {
	if env.Contains(id.Name.Value) {
		return newError("%s already defined", id.Name.String())
	}

	identType := id.Name.Type
	exprType := Check(id.Value, nil, env)

	if IsError(exprType) {
		return exprType
	}

	if identType == nil || identType.Equals(exprType) {
		env.SetNewObjectByType(id.Name.Value, exprType)
		identType = exprType
		return ast.UNIT
	} else {
		return typeError(id.Value, exprType, identType)
	}
}

func checkIdentDeclaration(id *ast.IdentDeclaration, env *ast.Environment) ast.TypeIdentifier {
	if env.Contains(id.Name.Value) {
		return newError("%s already defined", id.Name.String())
	}
	env.SetNewObjectByType(id.Name.Value, id.Name.Type)
	return ast.UNIT
}

func checkIdentAssign(ia *ast.IdentAssign, env *ast.Environment) ast.TypeIdentifier {
	identType, ok := env.GetObjectType(ia.Name.Value)

	if !ok {
		return undefinedError(ia.Name.Value)
	}

	exprType := Check(ia.Value, nil, env)
	if IsError(exprType) {
		return exprType
	}

	if !identType.Equals(exprType) {
		return typeError(ia.Value, exprType, identType)
	}

	return ast.UNIT
}

func checkBlockStatement(stmts []ast.Statement, expectedReturnType *ast.TypeReturnStatement, env *ast.Environment) (ast.TypeIdentifier, *ast.Environment) {
	var returnType ast.TypeIdentifier

	enclosedEnv := ast.NewEnclosedEnvironment(env)

	for _, stmt := range stmts {
		returnType = Check(stmt, expectedReturnType, enclosedEnv)

		if IsError(returnType) {
			return returnType, nil
		}
		returnType, ok := returnType.(*ast.TypeReturnStatement)
		if !ok || returnType.Cond {
			continue
		}
		if expectedReturnType == nil {
			return newError("unexpected return"), nil
		}
		if !returnType.Equals(expectedReturnType) {
			return typeError(stmt, returnType, expectedReturnType), nil
		}
		return returnType, enclosedEnv
	}

	return ast.UNIT, enclosedEnv
}

func checkIfConditions(conditions []ast.Expression, env *ast.Environment) ast.TypeIdentifier {
	for _, cond := range conditions {
		condType := Check(cond, nil, env)
		if !condType.Equals(ast.TYPE_BOOL) {
			return typeError(cond, condType, ast.TYPE_BOOL)
		}
	}
	return ast.TYPE_BOOL
}

func checkIfConsequences(
	consequences []*ast.BlockStatement,
	alternative *ast.BlockStatement,
	expectedReturnType *ast.TypeReturnStatement,
	env *ast.Environment,
) ast.TypeIdentifier {
	var consequenceType ast.TypeIdentifier
	for _, consequence := range consequences {
		consequenceType, consequence.Env = checkBlockStatement(
			consequence.Statements,
			expectedReturnType,
			env,
		)
		if IsError(consequenceType) {
			return consequenceType
		}
	}

	if alternative == nil {
		return ast.UNIT
	}

	var alternativeType ast.TypeIdentifier
	alternativeType, alternative.Env = checkBlockStatement(
		alternative.Statements,
		expectedReturnType,
		env,
	)

	return alternativeType
}

func checkStructFields(
	structFields map[string]ast.TypeIdentifier,
	assignedFields map[string]ast.Expression,
	env *ast.Environment,
) ast.TypeIdentifier {
	var assignedTi ast.TypeIdentifier
	for k, v := range assignedFields {
		structTi, ok := structFields[k]
		if !ok {
			return newError("field %s undefined", k)
		}
		assignedTi = Check(v, nil, env)
		if !structTi.Equals(assignedTi) {
			return typeError(v, assignedTi, structTi)
		}
	}
	return ast.UNIT
}

func checkListElements(
	internalType ast.TypeIdentifier,
	elements []ast.Expression,
	env *ast.Environment,
) ast.TypeIdentifier {
	var elemType ast.TypeIdentifier
	for _, e := range elements {
		elemType = Check(e, nil, env)
		if !elemType.Equals(internalType) {
			return typeError(e, elemType, internalType)
		}
	}
	return ast.UNIT
}

func checkMapKeys(
	keyType ast.TypeIdentifier,
	keys []ast.Expression,
	env *ast.Environment,
) ast.TypeIdentifier {
	var elemType ast.TypeIdentifier
	for _, e := range keys {
		_, ok := e.(ast.PrimitiveLiteral)
		if !ok {
			return newError("cannot use %s as %s literal", e, keyType)
		}
		elemType = Check(e, nil, env)
		if !elemType.Equals(keyType) {
			return typeError(e, elemType, keyType)
		}
	}
	return ast.UNIT
}

func checkModule(m *ast.Module) (ast.TypeIdentifier, *ast.Environment) {
	err := Check(m.Program, nil, m.Program.Environment)
	if IsError(err) {
		return err, nil
	}
	return ast.UNIT, m.Program.Environment
}

func typeError(expr ast.Node, exprType, expectedType ast.TypeIdentifier) ast.TypeIdentifier {
	return newError("cannot use %s (%s) as %s", expr, exprType, expectedType)
}

func undefinedError(identLiteral string) ast.TypeIdentifier {
	return newError("undefined: %s", identLiteral)
}

func newError(format string, a ...interface{}) ast.TypeIdentifier {
	return &ast.TypeCheckerError{Message: fmt.Sprintf(format, a...)}
}

func IsError(t ast.TypeIdentifier) bool {
	if t != nil {
		_, ok := t.(*ast.TypeCheckerError)
		return ok
	}
	return false
}
