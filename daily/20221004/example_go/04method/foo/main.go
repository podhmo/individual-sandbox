package main

import (
	"debug/elf"
	"debug/gosym"
	"fmt"
	"log"
	"os"
)

func main() {
	filename := os.Args[1]
	if err := run(filename); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run(filename string) error {
	f, err := elf.Open(filename)
	if err != nil {
		return err
	}
	defer f.Close()

	s := f.Section(".gosymtab")
	if s == nil {
		return fmt.Errorf("no .gosymtab section")
	}
	symdat, err := s.Data()
	if err != nil {
		return fmt.Errorf("ng gosymtab: %w", err)
	}
	pclndat, err := f.Section(".gopclntab").Data()
	if err != nil {
		return fmt.Errorf("ng gopclntab: %w", err)
	}

	pcln := gosym.NewLineTable(pclndat, f.Section(".text").Addr)
	tab, err := gosym.NewTable(symdat, pcln)
	if err != nil {
		return err
	}

	// for name := range tab.Files {
	// 	fmt.Println("@", name)
	// }
	for _, f := range tab.Funcs {
		if f.Func != nil && f.Func.PackageName() == "main" {
			fmt.Printf("%s\t%v\n", f.Name, f.Sym.Static())
		}
	}
	return nil
}
