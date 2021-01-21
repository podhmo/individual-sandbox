package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
)

func Run(d string) error {
	{
		f, err := os.Create(filepath.Join(d, "1.txt"))
		if err != nil {
			return err
		}
		fmt.Fprintln(f, "one")
	}
	{
		f, err := os.Create(filepath.Join(d, "2.txt"))
		if err != nil {
			return err
		}
		fmt.Fprintln(f, "two")
	}
	{
		f, err := os.Create(filepath.Join(d, "3.txt"))
		if err != nil {
			return err
		}
		fmt.Fprintln(f, "three")
	}
	return nil
}

func main() {
	if err := Run(os.Getenv("TARGET")); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
