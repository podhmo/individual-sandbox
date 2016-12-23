package main

// Resolver :
type Resolver interface {
	Resolve() int
}

// FoldResolver :
type FoldResolver struct {
	BOp  func(init int, n Resolver) int
	Args []Resolver
	init int
}

// Add :
func Add(args ...Resolver) Resolver {
	return &FoldResolver{
		BOp:  add2,
		Args: args,
		init: 0,
	}
}
func add2(x int, y Resolver) int {
	return x + y.Resolve()
}

// Sub :
func Sub(args ...Resolver) Resolver {
	return &FoldResolver{
		BOp:  sub2,
		Args: args,
		init: 0,
	}
}
func sub2(x int, y Resolver) int {
	return x - y.Resolve()
}

// Mul :
func Mul(args ...Resolver) Resolver {
	return &FoldResolver{
		BOp:  mul2,
		Args: args,
		init: 1,
	}
}
func mul2(x int, y Resolver) int {
	return x * y.Resolve()
}

// Div :
func Div(args ...Resolver) Resolver {
	return &FoldResolver{
		BOp:  div2,
		Args: args,
		init: 1,
	}
}
func div2(x int, y Resolver) int {
	return x / y.Resolve()
}

// Mod :
func Mod(args ...Resolver) Resolver {
	return &FoldResolver{
		BOp:  mod2,
		Args: args,
		init: 1,
	}
}
func mod2(x int, y Resolver) int {
	return x / y.Resolve()
}

// Init :
func (node *FoldResolver) Init() (int, []Resolver) {
	switch len(node.Args) {
	case 0:
		return node.init, []Resolver{}
	default:
		return node.Args[0].Resolve(), node.Args[1:]
	}
}

// Resolve :
func (node *FoldResolver) Resolve() int {
	value, rest := node.Init()
	for _, v := range rest {
		value = node.BOp(value, v)
	}
	return value
}

// Value :
type Value int

// Resolve :
func (node Value) Resolve() int {
	return int(node)
}

func main() {
}
