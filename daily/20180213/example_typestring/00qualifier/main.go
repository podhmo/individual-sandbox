package main

import (
	"fmt"
	"go/parser"
	"go/types"
	"log"

	"golang.org/x/tools/go/buildutil"
	"golang.org/x/tools/go/loader"
)

func main() {
	pkgs := map[string]map[string]string{
		"github.com/podhmo/sandbox/model": {
			"person.go": `
package model

import "time"

type Person struct {
	Name string
	Birth time.Time
}
`,
		},
		"time": {
			"time.go": `
package time

type Time string
`,
		},
	}
	conf := &loader.Config{
		Build:       buildutil.FakeContext(pkgs),
		ParserMode:  parser.ParseComments,
		AllowErrors: true, // xxx
	}

	conf.Import("github.com/podhmo/sandbox/model")
	prog, err := conf.Load()

	if err != nil {
		log.Fatal(err)
	}

	timePkg := prog.Package("time").Pkg
	modelPkg := prog.Package("github.com/podhmo/sandbox/model").Pkg

	{
		fmt.Println("in model package")
		fmt.Println("	", types.TypeString(
			timePkg.Scope().Lookup("Time").Type(),
			types.RelativeTo(modelPkg)))
		fmt.Println("	", types.TypeString(
			types.NewPointer(timePkg.Scope().Lookup("Time").Type()),
			types.RelativeTo(modelPkg)))
		fmt.Println("	", types.TypeString(
			modelPkg.Scope().Lookup("Person").Type(),
			types.RelativeTo(modelPkg)))
	}
	{
		fmt.Println("in time package")
		fmt.Println("	", types.TypeString(
			timePkg.Scope().Lookup("Time").Type(),
			types.RelativeTo(timePkg)))
		fmt.Println("	", types.TypeString(
			types.NewPointer(timePkg.Scope().Lookup("Time").Type()),
			types.RelativeTo(timePkg)))
		fmt.Println("	", types.TypeString(
			modelPkg.Scope().Lookup("Person").Type(),
			types.RelativeTo(timePkg)))
	}
}
