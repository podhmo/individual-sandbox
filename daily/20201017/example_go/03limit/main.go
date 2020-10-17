package main

import "fmt"

type ListParams struct {
	Limit  int
	Offset int
}
type SetLimit interface {
	SetLimit(int)
}

func (p *ListParams) SetLimit(limit int) {
	p.Limit = limit
}

type FooListParams struct {
	ListParams
}
type BarListParams struct {
	ListParams
}
type BooListParams struct {
	ListParams
}

type LimitOptFunc func(*ListParams)

func WithLimit(limit int) LimitOptFunc {
	return func(p *ListParams) {
		p.Limit = limit
	}
}

type OffsetOptFunc func(*ListParams)

func WithOffset(offset int) OffsetOptFunc {
	return func(p *ListParams) {
		p.Offset = offset
	}
}

type FooListOpt interface {
	ApplyFoo(*FooListParams)
}

func FooList(options ...FooListOpt) FooListParams {
	p := &FooListParams{}
	for _, opt := range options {
		opt.ApplyFoo(p)
	}
	return *p
}

func (f LimitOptFunc) ApplyFoo(p *FooListParams) {
	f(&p.ListParams)
}
func (f OffsetOptFunc) ApplyFoo(p *FooListParams) {
	f(&p.ListParams)
}

type BarListOpt interface {
	ApplyBar(*BarListParams)
}

func BarList(options ...BarListOpt) BarListParams {
	p := &BarListParams{}
	for _, opt := range options {
		opt.ApplyBar(p)
	}
	return *p
}

func (f LimitOptFunc) ApplyBar(p *BarListParams) {
	f(&p.ListParams)
}
func (f OffsetOptFunc) ApplyBar(p *BarListParams) {
	f(&p.ListParams)
}

func main() {
	fmt.Printf("%[1]T	%+[1]v\n", FooList(WithLimit(100)))
	fmt.Printf("%[1]T	%+[1]v\n", BarList(WithLimit(100), WithOffset(20)))
}
