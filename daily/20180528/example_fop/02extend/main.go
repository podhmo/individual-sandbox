package main

import "github.com/k0kubun/pp"

// F :
type F struct {
	x, y, z string
}

// G :
type G struct {
	x, y string
}

// H :
type H struct {
	x string
}

// BindF :
type BindF interface {
	F(*F)
}

// BindG :
type BindG interface {
	G(*G)
}

// BindH :
type BindH interface {
	H(*H)
}

// WithX :
func WithX(v string) interface {
	BindF
	BindG
	BindH
} {
	return &xclosure{v: v}
}

type xclosure struct{ v string }

func (x *xclosure) F(p *F) {
	p.x = x.v
}
func (x *xclosure) G(p *G) {
	p.x = x.v
}
func (x *xclosure) H(p *H) {
	p.x = x.v
}

// WithY :
func WithY(v string) interface {
	BindF
	BindG
} {
	return &yclosure{v: v}
}

type yclosure struct{ v string }

func (y *yclosure) F(p *F) {
	p.y = y.v
}
func (y *yclosure) G(p *G) {
	p.y = y.v
}

// WithZ :
func WithZ(v string) interface {
	BindF
} {
	return &zclosure{v: v}
}

type zclosure struct{ v string }

func (z *zclosure) F(p *F) {
	p.z = z.v
}

// NewF :
func NewF(bindings ...BindF) *F {
	f := &F{}
	for _, bind := range bindings {
		bind.F(f)
	}
	return f
}

// NewG :
func NewG(bindings ...BindG) *G {
	g := &G{}
	for _, bind := range bindings {
		bind.G(g)
	}
	return g
}

// NewH :
func NewH(bindings ...BindH) *H {
	h := &H{}
	for _, bind := range bindings {
		bind.H(h)
	}
	return h
}

func main() {
	{
		pp.Println(NewF(WithX("xxxx"), WithY("yyyy"), WithZ("zzzz")))
	}

	{
		// pp.Println(NewG(WithX("xxxx"), WithY("yyyy"), WithZ("zzzz"))) // compile error
		pp.Println(NewG(WithX("xxxx"), WithY("yyyy")))
	}

	{
		// pp.Println(NewH(WithX("xxxx"), WithY("yyyy"), WithZ("zzzz"))) // compile error
		// pp.Println(NewH(WithX("xxxx"), WithY("yyyy"))) // compile error
		pp.Println(NewH(WithX("xxxx")))
	}
}
