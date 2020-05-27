package mgox

type Uop struct { // unary
	Op    string
	Value interface{} // with converter?

	WithoutParen bool
}

type Bop struct { // binary
	Op    string
	Left  interface{}
	Right interface{}

	WithoutParen bool
}

type Mop struct { // multi
	Op     string
	Values []interface{}

	WithoutParen bool
}

type op interface {
	op()
}

func (*Uop) op() {}
func (*Bop) op() {}
func (*Mop) op() {}

// concrete
func And(values ...interface{}) *Mop {
	return &Mop{Op: "AND", Values: values}
}
func Or(values ...interface{}) *Mop {
	return &Mop{Op: "OR", Values: values}
}
func Not(value interface{}) *Uop {
	return &Uop{Op: "NOT", Value: value}
}
