package main

import (
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
	// 入力となるようなsource codeをコード内に含められる
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

	// 入力ファイルを複数にすることも可能だけれど。今はひとつだけ
	c.CreateFromFiles("./testcode", f)
	p, err := handwriting.New(
		"./testcode",
		handwriting.WithConfig(c),       // x/toolsのloader経由で*ast.Fileで作られたpackageを受け取れる
		handwriting.WithConsoleOutput(), // これをつけると出力は標準出力になる(debug print)
	)
	if err != nil {
		return err
	}

	// hello.goとbye.goの複数の出力が可能
	p.File("hello.go").Code(func(f *handwriting.File) error {
		f.Out.WithBlock("func Hello() string", func() {
			f.Out.Println(`return string(MessageHello)`)
		})
		return nil
	})

	p.File("bye.go").Code(func(f *handwriting.File) error {
		f.Out.WithBlock("func Bye() string", func() {
			f.Out.Println(`return string(MessageBye)`)
		})
		return nil
	})
	return p.Emit()
}
