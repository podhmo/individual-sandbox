package main

import (
	"errors"
	"fmt"
	"go/token"
	"go/types"
	"log"

	"github.com/podhmo/handwriting"
	"golang.org/x/tools/go/loader"
)

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	source := `
package testcode

type Message string
const (
	MessageHello = Message("HELLO")
	MessageBye = Message("BYE")
)
`
	c := &loader.Config{}
	f, err := c.ParseFile("message.go", source)
	if err != nil {
		return err
	}

	c.CreateFromFiles("./testcode", f)
	p, err := handwriting.New(
		"./testcode",
		handwriting.WithConfig(c),
		handwriting.WithConsoleOutput(),
	)
	if err != nil {
		return err
	}

	p.File("hello.go").Code(func(f *handwriting.File) error {
		message := f.PkgInfo.Pkg.Scope().Lookup("Message")
		if message == nil {
			return errors.New("message not found")
		}
		f.Out.WithBlock(fmt.Sprintf("func Hello() %s", message.Name()), func() {
			f.Out.Println(`return MessageHello`)
		})
		return nil
	})

	f2 := p.File("hello2.go")
	f2.ImportWithName("time", "xtime")
	f2.Code(func(f *handwriting.File) error {
		t := f.Prog.Package("time").Pkg.Scope().Lookup("Time")
		f.Out.WithBlock(fmt.Sprintf("func HelloWithNow(now %s) string", f.Resolver.TypeName(t.Type())), func() {
			f.Out.Printfln(`return fmt.Sprintf("%%s %%s\n", MessageHello, now)`)
		})
		return nil
	})

	f3 := p.File("hello3.go")
	f3.Import("time")
	f3.Code(func(f *handwriting.File) error {
		timepkg := f.Prog.Package("time").Pkg

		messageT := f.PkgInfo.Pkg.Scope().Lookup("Message").Type()
		timeT := timepkg.Scope().Lookup("Time").Type()
		retvalT := types.NewTuple(
			types.NewVar(token.NoPos, f.PkgInfo.Pkg, "", messageT),
			types.NewVar(token.NoPos, timepkg, "", timeT),
		)
		f.Out.WithBlock(fmt.Sprintf("func HelloWithNow2() %s", f.Resolver.TypeNameForResults(retvalT)), func() {
			f.Out.Println("now := time.Now()")
			f.Out.Println("return MessageHello, now")
		})
		return nil
	})

	p.File("bye.go").Code(func(f *handwriting.File) error {
		message := f.PkgInfo.Pkg.Scope().Lookup("Message")
		if message == nil {
			return errors.New("message not found")
		}
		f.Out.WithBlock(fmt.Sprintf("func Bye() %s", message.Name()), func() {
			f.Out.Println(`return string(MessageBye)`)
		})
		return nil
	})

	return p.Emit()
}
