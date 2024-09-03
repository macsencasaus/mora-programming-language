package evaluator

import (
	"fmt"
	"mora/ast"
)

func Eval(node ast.Node, env *ast.Environment) ast.ObjectValue {
	switch node := node.(type) {

	case *ast.Program:
		return evalProgram(node.Statements, env)

	case *ast.BoolLiteral:
		return ast.NativeBooleanObject(node.Value)

	case *ast.CharLiteral:
		return &ast.Char{Value: node.Value}

	case *ast.IntegerLiteral:
		return &ast.Integer{Value: node.Value}

	case *ast.StringLiteral:
		return &ast.String{Value: node.Value}

	case *ast.StructLiteral:
		return evalStructLiteral(node, env)

	case *ast.ListLiteral:
		return evalListLiteral(node, env)

	case *ast.ExpressionStatement:
		return Eval(node.Value, env)

	case *ast.Identifier:
		return evalIdentifier(node, env)

	case *ast.PrefixExpression:
		return evalPrefixExpression(node.Operator, Eval(node.Right, env))

	case *ast.CallExpression:
		return evalCallExpression(node, env)

	case *ast.FunctionDefinition:
		fn, ok := Eval(node.Call.Function, env).(*ast.Function)
		if !ok {
			return newError("function lacks accompanying binding")
		}
		if IsError(fn) {
			return fn
		}

	case *ast.IdentDeclaration:
		return evalIdentDeclaration(node, env)

	case *ast.IdentDefinition:
		return evalIdentDefinition(node, env)

	case *ast.IdentAssign:
		return evalIdentAssign(node, env)

	case *ast.BlockStatement:
		return evalBlockStatements(node)

	case *ast.ReturnStatement:
		return evalReturnStatement(node, env)

	case *ast.IfStatement:
		return evalIfStatement(node, env)

	case *ast.FieldExpression:
		return evalFieldExpression(node, env)

	case *ast.FieldDefinition:
		return evalFieldDefinition(node, env)

	case *ast.IndexExpression:
		return evalIndexExpression(node, env)

	case *ast.IndexDefinition:
		return evalIndexDefinition(node, env)

	case *ast.ImportStatement:
		moduleEnv, _ := env.GetModule(node.Module.Name)
		return Eval(node.Module.Program, moduleEnv)

	case *ast.FromStatement:
		return evalFromStatement(node, env)

	case *ast.UsingStatement:
		return evalUsingStatement(node, env)

	}
	return nil
}

func evalProgram(stmts []ast.Statement, env *ast.Environment) ast.ObjectValue {
	var result ast.ObjectValue

	for _, stmt := range stmts {
		result = Eval(stmt, env)
		if IsError(result) {
			return result
		}
	}

	return result
}

func evalStructLiteral(sl *ast.StructLiteral, env *ast.Environment) ast.ObjectValue {
	fields := make(map[string]ast.ObjectValue)

	for k, v := range sl.Fields {
		fields[k] = Eval(v, env)
	}

	return &ast.Struct{
		Name:   sl.Struct.String(),
		Fields: fields,
	}
}

func evalListLiteral(ll *ast.ListLiteral, env *ast.Environment) ast.ObjectValue {
	elements := make([]ast.ObjectValue, len(ll.Elements))

	for i, e := range ll.Elements {
		elements[i] = Eval(e, env)
	}

	return &ast.List{
		Name:     ll.List.String(),
		Elements: elements,
	}
}

func evalIdentifier(id *ast.Identifier, env *ast.Environment) ast.ObjectValue {
	o, _ := env.Get(id.Value)
	return o
}

func evalPrefixExpression(operator string, right ast.ObjectValue) ast.ObjectValue {
	switch operator {
	case "-":
		return evalMinusOperator(right)

	case "!":
		return evalBangOperator(right)

	default:
		return nil
	}
}

func evalMinusOperator(right ast.ObjectValue) ast.ObjectValue {
	switch right := right.(type) {
	case *ast.Integer:
		right.Value = -right.Value
		return right

	case *ast.Float:
		right.Value = -right.Value
		return right

	default:
		return nil
	}
}

func evalBangOperator(right ast.ObjectValue) ast.ObjectValue {
	switch right := right.(type) {
	case *ast.Boolean:
		return ast.NativeBooleanObject(!right.Value)
	default:
		return nil
	}
}

func evalCallExpression(
	callExp *ast.CallExpression,
	env *ast.Environment,
) ast.ObjectValue {
	fn, ok := Eval(callExp.Function, env).(*ast.Function)
	if !ok {
		return newError("function lacks accompanying binding1")
	}
	if IsError(fn) {
		return fn
	}

	arg := Eval(callExp.Argument, env)

	if IsError(arg) {
		return arg
	}
	res, ok := fn.Call(arg)
	if !ok {
		return newError("non-exhaustive pattern")
	}

	switch res := res.(type) {
	case *ast.Function:
		return res
	case *ast.FunctionBody:
		fbRes := Eval(res.Value, res.Env)
		if fRes, ok := fbRes.(*ast.ReturnValue); ok {
			return fRes.Value
		}
		return fbRes
	}
	return res
}

func evalIdentDeclaration(id *ast.IdentDeclaration, env *ast.Environment) ast.ObjectValue {
	ident := id.Name

	identName := ident.Value

	if ident.Type.Next() != nil {
		return nil
	}

	switch ident.Type.String() {
	case ast.PRIM_TYPE_BOOL_LITERAL:
		env.Set(identName, ast.NativeBooleanObject(false))
	case ast.PRIM_TYPE_INT_LITERAL:
		env.Set(identName, ast.ZERO)
	case ast.PRIM_TYPE_CHAR_LITERAL:
		env.Set(identName, ast.NULL_CHAR)
	case ast.PRIM_TYPE_STRING_LITERAL:
		env.Set(identName, &ast.List{Name: "[]char"})
	}

	return nil
}

func evalIdentDefinition(id *ast.IdentDefinition, env *ast.Environment) ast.ObjectValue {
	value := Eval(id.Value, env)
	env.Set(id.Name.Value, value)
	return nil
}

func evalIdentAssign(ia *ast.IdentAssign, env *ast.Environment) ast.ObjectValue {
	value := Eval(ia.Value, env)
	obj, _ := env.GetObject(ia.Name.Value)
	obj.Value = value
	return nil
}

func evalBlockStatements(bs *ast.BlockStatement) ast.ObjectValue {
	var result ast.ObjectValue

	for _, stmt := range bs.Statements {
		result = Eval(stmt, bs.Env)

		if IsError(result) || IsReturn(result) {
			return result
		}
	}

	return result
}

func evalReturnStatement(rs *ast.ReturnStatement, env *ast.Environment) ast.ObjectValue {
	if rs.Condition != nil {
		cond := Eval(rs.Value, env).(*ast.Integer)
		if cond.Value == 0 {
			return nil
		}
	}
	val := Eval(rs.Value, env)
	if IsError(val) {
		return val
	}
	return &ast.ReturnValue{Value: val}
}

func evalIfStatement(is *ast.IfStatement, env *ast.Environment) ast.ObjectValue {
	for idx, c := range is.Conditions {
		condition := Eval(c, env).(*ast.Integer)
		if condition.Value != 0 {
			return Eval(is.Consequences[idx], env)
		}
	}
	if is.Alternative != nil {
		return Eval(is.Alternative, env)
	}
	return nil
}

func evalFieldExpression(fe *ast.FieldExpression, env *ast.Environment) ast.ObjectValue {
	left := Eval(fe.Left, env)

	if IsError(left) {
		return left
	}

	switch left := left.(type) {
	case *ast.Struct:
		return left.Fields[fe.Field]

	case *ast.Module:
		moduleEnvironment, _ := env.GetModule(left.Name)
		val, _ := moduleEnvironment.Get(fe.Field)
		return val
	}

	return nil
}

func evalFieldDefinition(fd *ast.FieldDefinition, env *ast.Environment) ast.ObjectValue {
	value := Eval(fd.Value, env)
	if IsError(value) {
		return value
	}

	fe := fd.Field
	left := Eval(fe.Left, env)

	if IsError(left) {
		return left
	}

	switch left := left.(type) {
	case *ast.Struct:
		left.Fields[fe.Field] = value

		// TODO: modules
	}

	return nil
}

func evalIndexExpression(ie *ast.IndexExpression, env *ast.Environment) ast.ObjectValue {
	left := Eval(ie.Left, env)
	if IsError(left) {
		return left
	}

	idx := Eval(ie.Index, env)
	if IsError(idx) {
		return idx
	}

	switch left := left.(type) {
	case *ast.List:
		i := idx.(*ast.Integer).Value
		n := int64(len(left.Elements))
		if i >= 0 && i < n {
			return left.Elements[i]
		} else if i < 0 && -i <= n {
			return left.Elements[n+i]
		} else {
			return newError("index %d out of bounds for list of size %d", i, len(left.Elements))
		}

		// TODO: map
	}

	return nil
}

func evalIndexDefinition(id *ast.IndexDefinition, env *ast.Environment) ast.ObjectValue {
	value := Eval(id.Value, env)
	if IsError(value) {
		return value
	}

	ie := id.IndexExpression

	left := Eval(ie.Left, env)
	if IsError(left) {
		return left
	}

	idx := Eval(ie.Index, env)
	if IsError(idx) {
		return idx
	}

	switch left := left.(type) {
	case *ast.List:
		i := idx.(*ast.Integer).Value
		n := int64(len(left.Elements))
		if i >= 0 && i < n {
			left.Elements[i] = value
		} else if i < 0 && -i <= n {
			left.Elements[n+i] = value
		} else {
			return newError("index %d out of bounds for list of size %d", i, len(left.Elements))
		}

		// TODO: map
	}

	return nil
}

func evalFromStatement(node *ast.FromStatement, env *ast.Environment) ast.ObjectValue {
	moduleEnv, _ := env.GetModule(node.Module.Name)
	evaluatedModule := Eval(node.Module.Program, moduleEnv)

	if node.GetAll {
		for k, v := range moduleEnv.GetObjectStore() {
			env.Set(k, v.Value)
		}
		for k, v := range moduleEnv.GetTypeStore() {
			env.RegisterType(k, v)
		}
	}

	for _, ident := range node.Identifiers {
		obj, ok := moduleEnv.GetObject(ident)
		if !ok {
			ti, ok := moduleEnv.GetType(ident)
			if !ok {
				return newError("%s undefined in %s", ident, node.Module.Name)
			}
			env.RegisterType(ident, ti)
		}
		env.Set(ident, obj.Value)
	}

	return evaluatedModule
}

func evalUsingStatement(us *ast.UsingStatement, env *ast.Environment) ast.ObjectValue {
	fe := Eval(us.FieldedExpression, env)

	switch fe := fe.(type) {
	case *ast.Struct:
		for k, v := range fe.Fields {
			env.Set(k, v)
		}
	case *ast.Module:
		moduleEnvironment, _ := env.GetModule(fe.Name)
		for k, v := range moduleEnvironment.GetObjectStore() {
			env.Set(k, v.Value)
		}
	}

	return nil
}

func IsReturn(ov ast.ObjectValue) bool {
	_, ok := ov.(*ast.ReturnValue)
	return ok
}

func IsError(ov ast.ObjectValue) bool {
	_, ok := ov.(*ast.ObjectError)
	return ok
}

func newError(format string, a ...interface{}) ast.ObjectValue {
	return &ast.ObjectError{Message: fmt.Sprintf(format, a...)}
}
