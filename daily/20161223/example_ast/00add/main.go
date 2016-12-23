package main

// Resolver :
type Resolver interface {
	Resolve() int
}

// AddResolver :
type AddResolver struct {
	Args []Resolver
}

// Add :
func Add(args ...Resolver) Resolver {
    return &AddResolver{Args: args}
}

// Resolve :
func (node *AddResolver) Resolve() int {
	r := 0
	for _, v := range node.Args {
		r += v.Resolve()
	}
	return r
}

// Value :
type Value int

// Resolve :
func (node Value) Resolve() int {
	return int(node)
}

func main() {
}
