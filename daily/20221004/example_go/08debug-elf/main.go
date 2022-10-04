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
	textAddr := f.Section(".text").Addr

	if len(symdat) > 0 {
		log.Printf("symdat[:30] -> %v", symdat[:30])
		log.Printf("symdat[-30:] -> %v", symdat[len(symdat)-30:])
	}
	log.Printf(".text addr -> %x", textAddr)
	log.Printf("pclndat[:30] -> %x", pclndat[:30])
	log.Printf("pclndat[-30:] -> %x", pclndat[len(pclndat)-30:])

	pcln := gosym.NewLineTable(pclndat, textAddr)
	tab, err := gosym.NewTable(symdat, pcln)
	if err != nil {
		return err
	}

	for _, f := range tab.Funcs {
		if f.Func != nil && f.Func.PackageName() == "main" {
			fmt.Printf("%s\t%v\n", f.Name, f.Sym.Static())
		}
	}
	return nil
}
