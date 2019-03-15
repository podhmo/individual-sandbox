package main

import (
	"log"
	"runtime"

	"github.com/k0kubun/pp"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	fn := func() {
		{
			pc, file, line, ok := runtime.Caller(0)
			fn := runtime.FuncForPC(pc)
			fnName := fn.Name()
			fnFile, fnLine := fn.FileLine(pc)
			fnEntry := fn.Entry()
			pp.Println(pc, file, line, ok, fnName, fnFile, fnLine, fnEntry)
		}
		{
			pc, file, line, ok := runtime.Caller(1)
			fn := runtime.FuncForPC(pc)
			fnName := fn.Name()
			fnFile, fnLine := fn.FileLine(pc)
			fnEntry := fn.Entry()
			pp.Println(pc, file, line, ok, fnName, fnFile, fnLine, fnEntry)
		}
	}
	fn()

	{
		pc, file, line, ok := runtime.Caller(1)
		fn := runtime.FuncForPC(pc)
		fnName := fn.Name()
		fnFile, fnLine := fn.FileLine(pc)
		fnEntry := fn.Entry()
		pp.Println(pc, file, line, ok, fnName, fnFile, fnLine, fnEntry)
	}
	return nil
}
