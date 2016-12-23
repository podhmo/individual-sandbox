package main

import "strings"

// Resolver :
type Resolver interface {
	Resolve() Atom
}

// Atom :
type Atom interface {
	Zero() Atom
}

// BOp :
type BOp func(x Atom, y Resolver) Atom

// HasAdd :
type HasAdd interface {
	Add() BOp
}

// HasSub :
type HasSub interface {
	Sub() BOp
}

// HasMul :
type HasMul interface {
	Mul() BOp
}

// Int :
type Int int

// Add :
func (z Int) Add() BOp {
	return func(x Atom, y Resolver) Atom {
		return Int(int(x.(Int)) + int(y.Resolve().(Int)))
	}
}

// Sub :
func (z Int) Sub() BOp {
	return func(x Atom, y Resolver) Atom {
		return Int(int(x.(Int)) - int(y.Resolve().(Int)))
	}
}

// Mul :
func (z Int) Mul() BOp {
	return func(x Atom, y Resolver) Atom {
		return Int(int(x.(Int)) * int(y.Resolve().(Int)))
	}
}

// Resolve :
func (z Int) Resolve() Atom {
	return z
}

// Zero :
func (z Int) Zero() Atom {
	return Int(0)
}

// String :
type String string

// Add :
func (z String) Add() BOp {
	return func(x Atom, y Resolver) Atom {
		return String(string(x.(String)) + string(y.(String)))
	}
}

// Sub :
func (z String) Sub() BOp {
	return func(x Atom, y Resolver) Atom {
		return String(string(x.(String))[int(y.Resolve().(Int)):])
	}
}

// Mul :
func (z String) Mul() BOp {
	return func(x Atom, y Resolver) Atom {
		return String(strings.Repeat(string(x.(String)), int(y.Resolve().(Int))))
	}
}

// Resolve :
func (z String) Resolve() Atom {
	return z
}

// Zero :
func (z String) Zero() Atom {
	return String("")
}

// FoldResolver :
type FoldResolver struct {
	Args []Resolver
	Init Resolver
	BOp  BOp
}

// Add :
func Add(init Resolver, args ...Resolver) Resolver {
	return &FoldResolver{
		Args: args,
		Init: init,
		BOp:  init.(HasAdd).Add(),
	}
}

// Sub :
func Sub(init Resolver, args ...Resolver) Resolver {
	return &FoldResolver{
		Args: args,
		Init: init,
		BOp:  init.(HasSub).Sub(),
	}
}

// Mul :
func Mul(init Resolver, args ...Resolver) Resolver {
	return &FoldResolver{
		Args: args,
		Init: init,
		BOp:  init.(HasMul).Mul(),
	}
}

// Resolve :
func (node *FoldResolver) Resolve() Atom {
	value := node.Init.Resolve()
	for _, v := range node.Args {
		value = node.BOp(value, v)
	}
	return value
}

// Add :
func (node *FoldResolver) Add() BOp {
	return node.Init.(HasAdd).Add()
}

// Sub :
func (node *FoldResolver) Sub() BOp {
	return node.Init.(HasSub).Sub()
}

// Mul :
func (node *FoldResolver) Mul() BOp {
	return node.Init.(HasMul).Mul()
}

func main() {
}
