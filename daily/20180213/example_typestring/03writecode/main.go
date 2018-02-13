package main

import (
	"fmt"
	"go/build"
	"os/user"
	"strings"
)

func main() {
	ctxt := &build.Default
	{
		pkg, _ := ctxt.Import(".", ".", build.FindOnly)
		fmt.Println(pkg.Dir)
	}
	{
		pkg, _ := ctxt.Import("fmt", ".", build.FindOnly)
		u, _ := user.Current()
		fmt.Println(strings.Replace(pkg.Dir, u.HomeDir, "~", 1))
	}
	{
		pkg, _ := ctxt.Import("golang.org/x/tools/go/loader", ".", build.FindOnly)
		u, _ := user.Current()
		fmt.Println(strings.Replace(pkg.Dir, u.HomeDir, "~", 1))
	}
}
