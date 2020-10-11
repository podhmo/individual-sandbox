package main

import (
	"io"
	"strconv"
	"strings"
	"testing"
)

type Path []Node

func (p Path) String() string {
	if len(p) == 0 {
		return ""
	}

	var b strings.Builder
	io.WriteString(&b, p[0].val)
	for _, x := range p[1:] {
		io.WriteString(&b, x.prefix+x.val+x.suffix)
	}
	return b.String()
}

type Node struct {
	prefix string
	val    string
	suffix string
}

func String(v string) Node {
	return Node{prefix: ".", val: v}
}
func Int(v int) Node {
	return Node{prefix: "[", val: strconv.Itoa(v), suffix: "]"}
}

type Path2 []Node2
type Node2 interface {
	String() string
	StringWithAccessor() string
}

type String2 string

func (v String2) String() string {
	return string(v)
}
func (v String2) StringWithAccessor() string {
	return "." + string(v)
}

type Int2 int

func (v Int2) String() string {
	return strconv.Itoa(int(v))
}
func (v Int2) StringWithAccessor() string {
	return "[" + strconv.Itoa(int(v)) + "]"
}
func (p Path2) String() string {
	if len(p) == 0 {
		return ""
	}

	var b strings.Builder
	io.WriteString(&b, p[0].String())
	for _, x := range p[1:] {
		io.WriteString(&b, x.StringWithAccessor())
	}
	return b.String()
}

func BenchmarkPath(b *testing.B) {
	b.ResetTimer()
	{
		var p Path
		p = append(p, String("foo"))
		p = append(p, String("bar"))
		p = append(p, String("boo"))
		p.String()
		p.String()
		p.String()
	}
	{
		var p Path
		p = append(p, String("foo"))
		p = append(p, Int(10))
		p = append(p, String("bar"))
		p = append(p, Int(20))
		p = append(p, String("boo"))
		p.String()
		p.String()
		p.String()
	}
}

func BenchmarkPath2(b *testing.B) {
	b.ResetTimer()

	var p Path2
	p = append(p, String2("foo"))
	p = append(p, String2("bar"))
	p = append(p, String2("boo"))
	p.String()
	p.String()
	p.String()

	{
		var p Path2
		p = append(p, String2("foo"))
		p = append(p, Int2(10))
		p = append(p, String2("bar"))
		p = append(p, Int2(20))
		p = append(p, String2("boo"))
		p.String()
		p.String()
		p.String()
	}
}
