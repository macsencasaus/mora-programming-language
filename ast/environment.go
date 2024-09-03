package ast

type Environment struct {
	objectStore map[string]*Object
	typeStore   map[string]TypeIdentifier
	outer       *Environment
	modules     map[string]*Environment
}

func NewEnvironment() *Environment {
	return &Environment{
		objectStore: make(map[string]*Object),
		typeStore:   make(map[string]TypeIdentifier),
		outer:       nil,
		modules:     make(map[string]*Environment),
	}
}

func NewEnclosedEnvironment(outer *Environment) *Environment {
	env := NewEnvironment()
	env.outer = outer
	return env
}

func (e *Environment) GetObject(name string) (*Object, bool) {
	obj, ok := e.objectStore[name]
	if !ok && e.outer != nil {
		obj, ok = e.outer.GetObject(name)
	}
	return obj, ok
}

func (e *Environment) Get(name string) (ObjectValue, bool) {
	obj, ok := e.GetObject(name)
	if !ok {
		return nil, ok
	}
	return obj.Value, ok
}

func (e *Environment) GetObjectType(name string) (TypeIdentifier, bool) {
	obj, ok := e.GetObject(name)
	if !ok {
		return nil, ok
	}
	return obj.Type, ok
}

func (e *Environment) GetModule(moduleName string) (*Environment, bool) {
	moduleEnv, ok := e.modules[moduleName]
	if !ok && e.outer != nil {
		moduleEnv, ok = e.outer.GetModule(moduleName)
	}
	return moduleEnv, ok
}

func (e *Environment) SetModule(moduleName string, moduleEnv *Environment) *Environment {
	e.modules[moduleName] = moduleEnv
	return moduleEnv
}

func (e *Environment) Set(name string, val ObjectValue) ObjectValue {
	e.objectStore[name].Value = val
	return val
}

func (e *Environment) SetObject(name string, val *Object) *Object {
	e.objectStore[name] = val
	return val
}

func (e *Environment) SetNewObjectByType(name string, t TypeIdentifier) *Object {
	obj := NewObjectByType(t)
	e.objectStore[name] = obj
	return obj
}

func (e *Environment) Contains(name string) bool {
	_, ok := e.objectStore[name]
	return ok
}

func (e *Environment) GetType(name string) (TypeIdentifier, bool) {
	t, ok := e.typeStore[name]
	if !ok && e.outer != nil {
		t, ok = e.outer.GetType(name)
	}
	return t, ok
}

func (e *Environment) RegisterType(newType string, aliasType TypeIdentifier) {
	switch aliasType := aliasType.(type) {
	case *PrimitiveTypeIdentifier:
		aliasType.Name = newType
	case *TypeStruct:
		aliasType.Name = newType
	case *TypeList:
		aliasType.Name = newType
	}
	e.typeStore[newType] = aliasType
}

func (e *Environment) GetObjectStore() map[string]*Object {
	return e.objectStore
}

func (e *Environment) GetTypeStore() map[string]TypeIdentifier {
	return e.typeStore
}
