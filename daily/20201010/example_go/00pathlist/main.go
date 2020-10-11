package main

import (
	"fmt"
	"io"
	"strconv"
	"strings"
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
