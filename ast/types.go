package ast

import (
	"bytes"
)

type TypeIdentifier interface {
	typeIdentifier()
	String() string
	Next() TypeIdentifier
	DetachAndCopy() TypeIdentifier
	DeepCopy() TypeIdentifier
	AppendNode(node TypeIdentifier) TypeIdentifier
	Equals(ti TypeIdentifier) bool
	Reduce(args ...TypeIdentifier) TypeIdentifier
}

type UnitTypeIdentifier struct{}

var UNIT *UnitTypeIdentifier

func (uti *UnitTypeIdentifier) typeIdentifier()               {}
func (uti *UnitTypeIdentifier) String() string                { return "()" }
func (uti *UnitTypeIdentifier) Next() TypeIdentifier          { return nil }
func (uti *UnitTypeIdentifier) DetachAndCopy() TypeIdentifier { return uti }
func (uti *UnitTypeIdentifier) DeepCopy() TypeIdentifier      { return uti }
func (uti *UnitTypeIdentifier) AppendNode(_ TypeIdentifier) TypeIdentifier {
	return &TypeCheckerError{Message: "cannot append node to unit"}
}
func (uti *UnitTypeIdentifier) Equals(ti TypeIdentifier) bool             { return uti == ti }
func (uti *UnitTypeIdentifier) Reduce(_ ...TypeIdentifier) TypeIdentifier { return nil }

type PrimitiveTypeIdentifier struct {
	Name  string
	Value string
	next  TypeIdentifier
}

func (pti *PrimitiveTypeIdentifier) typeIdentifier() {}
func (pti *PrimitiveTypeIdentifier) String() string {
	var out bytes.Buffer

	if pti.Name != "" {
		out.WriteString(pti.Name)
	} else {
		out.WriteString(pti.Value)
	}

	if pti.next != nil {
		out.WriteString(" -> ")
		out.WriteString(pti.next.String())
	}

	return out.String()
}
func (pti *PrimitiveTypeIdentifier) Next() TypeIdentifier { return pti.next }
func (pti *PrimitiveTypeIdentifier) DetachAndCopy() TypeIdentifier {
	return &PrimitiveTypeIdentifier{Name: pti.Name, Value: pti.Value}
}
func (pti *PrimitiveTypeIdentifier) DeepCopy() TypeIdentifier {
	if pti.next == nil {
		return pti.DetachAndCopy()
	}
	return &PrimitiveTypeIdentifier{Name: pti.Name, Value: pti.Value, next: pti.next.DeepCopy()}
}
func (pti *PrimitiveTypeIdentifier) AppendNode(node TypeIdentifier) TypeIdentifier {
	pti.next = node
	return node
}
func (pti *PrimitiveTypeIdentifier) equalsNode(ti TypeIdentifier) bool {
	if ti.Next() != nil {
		return false
	}
	otherPti, ok := ti.(*PrimitiveTypeIdentifier)
	return ok && pti.Value == otherPti.Value
}
func (pti *PrimitiveTypeIdentifier) Equals(ti TypeIdentifier) bool {
	otherPti, ok := ti.(*PrimitiveTypeIdentifier)
	if !ok || pti.Value != otherPti.Value {
		return false
	}

	if pti.Next() == nil && ti.Next() == nil {
		return true
	}

	if pti.Next() == nil || ti.Next() == nil {
		return false
	}

	return pti.Next().Equals(ti.Next())
}
func (pti *PrimitiveTypeIdentifier) Reduce(args ...TypeIdentifier) TypeIdentifier {
	if len(args) == 0 {
		return pti
	}

	if !pti.equalsNode(args[0]) {
		return nil
	}

	if pti.Next() == nil {
		return nil
	}

	return pti.Next().Reduce(args[1:]...)
}

type GenericTypeIdentifier struct {
	Name string
	next TypeIdentifier
}

func (gti *GenericTypeIdentifier) typeIdentifier() {}
func (gti *GenericTypeIdentifier) String() string {
	var out bytes.Buffer

	out.WriteString(gti.Name)

	if gti.next != nil {
		out.WriteString(" -> ")
		out.WriteString(gti.next.String())
	}

	return out.String()
}
func (gti *GenericTypeIdentifier) Next() TypeIdentifier {
	return gti.next
}
func (gti *GenericTypeIdentifier) DetachAndCopy() TypeIdentifier {
	return &GenericTypeIdentifier{Name: gti.Name}
}
func (gti *GenericTypeIdentifier) DeepCopy() TypeIdentifier {
	return &GenericTypeIdentifier{Name: gti.Name, next: gti.next.DeepCopy()}
}
func (gti *GenericTypeIdentifier) AppendNode(node TypeIdentifier) TypeIdentifier {
	gti.next = node
	return node
}
func (gti *GenericTypeIdentifier) Equals(ti TypeIdentifier) bool {
	otherGti, ok := ti.(*GenericTypeIdentifier)
	if !ok || gti.Name != otherGti.Name {
		return false
	}

	if gti.Next() == nil && ti.Next() == nil {
		return true
	}

	if gti.Next() == nil || ti.Next() == nil {
		return false
	}

	return gti.Next().Equals(ti.Next())
}
func (gti *GenericTypeIdentifier) Reduce(args ...TypeIdentifier) TypeIdentifier {
	// TODO:
	if len(args) == 0 {
		return gti
	}
	return nil
}

type FunctionTypeIdentifier struct {
	Head TypeIdentifier
	next TypeIdentifier
}

func (fti *FunctionTypeIdentifier) typeIdentifier() {}
func (fti *FunctionTypeIdentifier) Next() TypeIdentifier {
	return fti.next
}
func (fti *FunctionTypeIdentifier) DetachAndCopy() TypeIdentifier {
	return fti.Head
}
func (fti *FunctionTypeIdentifier) DeepCopy() TypeIdentifier {
	if fti.next == nil {
		return &FunctionTypeIdentifier{Head: fti.Head}
	}
	return &FunctionTypeIdentifier{
		Head: fti.Head,
		next: fti.next.DeepCopy(),
	}
}
func (fti *FunctionTypeIdentifier) AppendNode(node TypeIdentifier) TypeIdentifier {
	fti.next = node
	return node
}
func (fti *FunctionTypeIdentifier) equalsNode(ti TypeIdentifier) bool {
	pti, ok := ti.(*PrimitiveTypeIdentifier)
	if !ok || !fti.Head.Equals(pti) {
		return false
	}

	return true
}
func (fti *FunctionTypeIdentifier) Equals(ti TypeIdentifier) bool {
	otherFti, ok := ti.(*FunctionTypeIdentifier)
	if !ok || !fti.Head.Equals(otherFti.Head) {
		return false
	}

	if fti.Next() == nil && ti.Next() == nil {
		return true
	}

	if fti.Next() == nil || ti.Next() == nil {
		return false
	}

	return fti.Next().Equals(ti.Next())
}
func (fti *FunctionTypeIdentifier) Reduce(args ...TypeIdentifier) TypeIdentifier {
	if len(args) == 0 {
		return fti
	}

	if !fti.Head.Equals(args[0]) {
		return nil
	}

	if fti.Next() == nil {
		return nil
	}

	return fti.Next().Reduce(args[1:]...)
}
func (fti *FunctionTypeIdentifier) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(fti.Head.String())
	out.WriteString(")")

	if fti.next != nil {
		out.WriteString(" -> ")
		out.WriteString(fti.next.String())
	}

	return out.String()
}

type TypeStruct struct {
	Name   string
	Fields map[string]TypeIdentifier
	next   TypeIdentifier
}

func (ts *TypeStruct) typeIdentifier() {}
func (ts *TypeStruct) String() string {
	var out bytes.Buffer

	if ts.Name != "" {
		out.WriteString(ts.Name)
	} else {
		out.WriteString("struct {\n")
		for k, v := range ts.Fields {
			out.WriteString("    ")
			out.WriteString(k)
			out.WriteString(": ")
			out.WriteString(v.String())
			out.WriteString(";\n")
		}
		out.WriteString("}")
	}

	if ts.next != nil {
		out.WriteString(" -> ")
		out.WriteString(ts.next.String())
	}

	return out.String()
}
func (ts *TypeStruct) Next() TypeIdentifier { return ts.next }
func (ts *TypeStruct) DetachAndCopy() TypeIdentifier {
	return &TypeStruct{Name: ts.Name, Fields: ts.Fields}
}
func (ts *TypeStruct) DeepCopy() TypeIdentifier {
	if ts.next == nil {
		return ts.DetachAndCopy()
	}
	return &TypeStruct{Name: ts.Name, Fields: ts.Fields, next: ts.next.DeepCopy()}
}
func (ts *TypeStruct) AppendNode(node TypeIdentifier) TypeIdentifier {
	ts.next = node
	return node
}
func (ts *TypeStruct) equalsNode(ti TypeIdentifier) bool {
	otherTs, ok := ti.(*TypeStruct)
	if !ok {
		return false
	}

	if len(ts.Fields) != len(otherTs.Fields) {
		return false
	}

	for i, field := range ts.Fields {
		otherField, ok := otherTs.Fields[i]
		if !ok {
			return false
		}
		if !field.Equals(otherField) {
			return false
		}
	}
	return true
}
func (ts *TypeStruct) Equals(ti TypeIdentifier) bool {
	if !ts.equalsNode(ti) {
		return false
	}

	if ts.Next() == nil && ti.Next() == nil {
		return true
	}

	if ts.Next() == nil || ti.Next() == nil {
		return false
	}

	return ts.Next().Equals(ti.Next())
}
func (ts *TypeStruct) Reduce(args ...TypeIdentifier) TypeIdentifier {
	if len(args) == 0 {
		return ts
	}

	if !ts.equalsNode(args[0]) {
		return nil
	}

	if ts.Next() == nil {
		return nil
	}

	return ts.Next().Reduce(args[1:]...)
}

type TypeList struct {
	Name      string
	EntryType TypeIdentifier
	next      TypeIdentifier
}

func (tl *TypeList) typeIdentifier() {}
func (tl *TypeList) String() string {
	var out bytes.Buffer

	if tl.Name != "" {
		out.WriteString(tl.Name)
	} else {
		out.WriteString("[]")
		out.WriteString(tl.EntryType.String())
	}

	if tl.next != nil {
		out.WriteString(" -> ")
		out.WriteString(tl.next.String())
	}

	return out.String()
}
func (tl *TypeList) Next() TypeIdentifier { return tl.next }
func (tl *TypeList) DetachAndCopy() TypeIdentifier {
	return &TypeList{Name: tl.Name, EntryType: tl.EntryType}
}
func (tl *TypeList) DeepCopy() TypeIdentifier {
	if tl.next == nil {
		return tl.DetachAndCopy()
	}
	return &TypeList{
		Name:      tl.Name,
		EntryType: tl.EntryType,
		next:      tl.next,
	}
}
func (tl *TypeList) AppendNode(node TypeIdentifier) TypeIdentifier {
	tl.next = node
	return node
}
func (tl *TypeList) equalsNode(ti TypeIdentifier) bool {
	if ti.Next() != nil {
		return false
	}
	otherTl, ok := ti.(*TypeList)
	return ok && tl.EntryType.Equals(otherTl.EntryType)
}
func (tl *TypeList) Equals(ti TypeIdentifier) bool {
	otherTl, ok := ti.(*TypeList)
	if !ok || !tl.EntryType.Equals(otherTl.EntryType) {
		return false
	}

	if tl.Next() == nil && otherTl.Next() == nil {
		return true
	}

	if tl.Next() == nil || otherTl.Next() == nil {
		return false
	}

	return tl.Next().Equals(tl.Next())
}
func (tl *TypeList) Reduce(args ...TypeIdentifier) TypeIdentifier {
	if len(args) == 0 {
		return tl
	}

	if !tl.equalsNode(args[0]) {
		return nil
	}

	if tl.Next() == nil {
		return nil
	}

	return tl.Next().Reduce(args[1:]...)
}

type TypeMap struct {
	Name      string
	KeyType   TypeIdentifier
	ValueType TypeIdentifier
	next      TypeIdentifier
}

func (tm *TypeMap) typeIdentifier() {}
func (tm *TypeMap) String() string {
	var out bytes.Buffer

	if tm.Name != "" {
		out.WriteString(tm.Name)
	} else {
		out.WriteString("map[")
		out.WriteString(tm.KeyType.String())
		out.WriteString("]")
		out.WriteString(tm.ValueType.String())
	}

	if tm.next != nil {
		out.WriteString(tm.next.String())
	}

	return out.String()
}
func (tm *TypeMap) Next() TypeIdentifier { return tm.next }
func (tm *TypeMap) DetachAndCopy() TypeIdentifier {
	return &TypeMap{
		Name:      tm.Name,
		KeyType:   tm.KeyType,
		ValueType: tm.ValueType,
	}
}
func (tm *TypeMap) DeepCopy() TypeIdentifier {
	if tm.next == nil {
		return tm.DetachAndCopy()
	}
	return &TypeMap{
		Name:      tm.Name,
		KeyType:   tm.KeyType,
		ValueType: tm.ValueType,
		next:      tm.next.DeepCopy(),
	}
}
func (tm *TypeMap) AppendNode(node TypeIdentifier) TypeIdentifier {
	tm.next = node
	return node
}
func (tm *TypeMap) equalsNode(ti TypeIdentifier) bool {
	if tm.next != nil {
		return false
	}
	otherTm, ok := ti.(*TypeMap)
	return ok &&
		tm.KeyType.Equals(otherTm.KeyType) &&
		tm.ValueType.Equals(otherTm.ValueType)
}
func (tm *TypeMap) Equals(ti TypeIdentifier) bool {
	if !tm.equalsNode(ti) {
		return false
	}

	if tm.Next() == nil && ti.Next() == nil {
		return true
	}

	if tm.Next() == nil || ti.Next() == nil {
		return false
	}

	return tm.Next().Equals(ti.Next())
}
func (tm *TypeMap) Reduce(args ...TypeIdentifier) TypeIdentifier {
	if len(args) == 0 {
		return tm
	}

	if !tm.equalsNode(args[0]) {
		return nil
	}

	if tm.Next() == nil {
		return nil
	}

	return tm.Next().Reduce(args[1:]...)
}

type TypeModule struct {
	Name string
}

func (tm *TypeModule) typeIdentifier()                            {}
func (tm *TypeModule) String() string                             { return "" }
func (tm *TypeModule) Next() TypeIdentifier                       { return nil }
func (tm *TypeModule) DetachAndCopy() TypeIdentifier              { return nil }
func (tm *TypeModule) DeepCopy() TypeIdentifier                   { return nil }
func (tm *TypeModule) AppendNode(_ TypeIdentifier) TypeIdentifier { return nil }
func (tm *TypeModule) Equals(_ TypeIdentifier) bool               { return false }
func (tm *TypeModule) Reduce(_ ...TypeIdentifier) TypeIdentifier  { return nil }

type TypeCheckerError struct {
	Message string
}

func (tce *TypeCheckerError) typeIdentifier()                            {}
func (tce *TypeCheckerError) String() string                             { return tce.Message }
func (tce *TypeCheckerError) Next() TypeIdentifier                       { return nil }
func (tce *TypeCheckerError) DetachAndCopy() TypeIdentifier              { return tce }
func (tce *TypeCheckerError) DeepCopy() TypeIdentifier                   { return tce }
func (tce *TypeCheckerError) AppendNode(_ TypeIdentifier) TypeIdentifier { return nil }
func (tce *TypeCheckerError) Equals(_ TypeIdentifier) bool               { return false }
func (tce *TypeCheckerError) Reduce(_ ...TypeIdentifier) TypeIdentifier  { return nil }

type TypeReturnStatement struct {
	Value TypeIdentifier
	Cond  bool
}

func (trs *TypeReturnStatement) typeIdentifier()                            {}
func (trs *TypeReturnStatement) String() string                             { return "return " + trs.Value.String() }
func (trs *TypeReturnStatement) Next() TypeIdentifier                       { return nil }
func (trs *TypeReturnStatement) DetachAndCopy() TypeIdentifier              { return trs }
func (trs *TypeReturnStatement) DeepCopy() TypeIdentifier                   { return trs }
func (tce *TypeReturnStatement) AppendNode(_ TypeIdentifier) TypeIdentifier { return nil }
func (trs *TypeReturnStatement) Equals(ti TypeIdentifier) bool {
	tr, ok := ti.(*TypeReturnStatement)
	if !ok {
		return false
	}
	return trs.Value.Equals(tr.Value)
}
func (trs *TypeReturnStatement) Reduce(_ ...TypeIdentifier) TypeIdentifier { return nil }
