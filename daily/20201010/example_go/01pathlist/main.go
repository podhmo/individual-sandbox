package main

import (
	"fmt"
	"io"
	"strconv"
	"strings"
)

type Path []Node
type Node interface {
	String() string
	StringWithAccessor() string
}

type String string

func (v String) String() string {
	return string(v)
}
func (v String) StringWithAccessor() string {
	return "." + string(v)
}

type Int int

func (v Int) String() string {
	return strconv.Itoa(int(v))
}
func (v Int) StringWithAccessor() string {
	return "[" + strconv.Itoa(int(v)) + "]"
}
func (p Path) String() string {
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

func main() {
	{
		var p Path
		p = append(p, String("foo"))
		p = append(p, String("bar"))
		p = append(p, String("boo"))
		fmt.Println(p)
	}
	{
		var p Path
		p = append(p, String("foo"))
		p = append(p, Int(10))
		p = append(p, String("bar"))
		p = append(p, String("boo"))
		fmt.Println(p)
	}
}
