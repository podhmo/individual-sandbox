package main

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
)

type rawString string
type Stringer interface {
	String() string
}

func (x rawString) String() string {
	return string(x)
}

type Name struct {
	Value *string
}

func (x Name) String() string {
	return *x.Value
}

type Dotted struct {
	Recv  Name
	Value Name
}

func (x Dotted) String() string {
	return x.Recv.String() + "." + x.Value.String()
}

type Call struct {
	Name Name
	Args []Stringer
}

func (x Call) String() string {
	args := make([]string, len(x.Args))
	for i, x := range x.Args {
		args[i] = x.String()
	}
	return fmt.Sprintf("%s(%s)", x.Name, strings.Join(args, ", "))
}

type Assign struct {
	Lhs Stringer
	Rhs Stringer
}

func (x Assign) String() string {
	return fmt.Sprintf("%s := %s", x.Lhs, x.Rhs)
}

type VariableAccessor struct {
	names   map[string]Name
	removed map[string]Name
}

func (a *VariableAccessor) Replace(from, to string) bool {
	v, ok := a.names[from]
	if !ok {
		return false
	}

	delete(a.names, from)
	a.removed[from] = v

	a.names[to] = v
	*v.Value = to
	return true
}

func (a *VariableAccessor) List() []string {
	r := make([]string, 0, len(a.names))
	for _, x := range a.names {
		r = append(r, *x.Value)
	}
	sort.Strings(r)
	return r
}

type Env struct {
	Variables *VariableAccessor
}

func New() *Env {
	return &Env{
		Variables: &VariableAccessor{
			names:   map[string]Name{},
			removed: map[string]Name{},
		},
	}
}
func (e *Env) Name(name string) Name {
	names := e.Variables.names
	if x, ok := names[name]; ok {
		return x
	}
	removed := e.Variables.removed
	if x, ok := removed[name]; ok {
		delete(removed, name)
		names[name] = x // xxx
		return x
	}
	x := Name{Value: &name}
	names[name] = x
	return x
}
func (e *Env) Assign(lhs, rhs Stringer) Assign {
	return Assign{Lhs: lhs, Rhs: rhs}
}
func (e *Env) String(v string) rawString {
	return rawString(strconv.Quote(v))
}

func main() {
	e := New()
	fmt.Println(e.Assign(e.Name("foo"), e.String("bar")))
	e.Variables.Replace("foo", "xxx")
	fmt.Println(e.Assign(e.Name("foo"), e.String("bar")))

	fmt.Println("variables ...", e.Variables.List())
}
