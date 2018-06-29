package main

import (
	"fmt"
	"image"
	_ "image/gif"
	_ "image/jpeg"
	_ "image/png"
	"log"
	"os"
	"path/filepath"
)

func main() {
	if err := run(os.Args[1]); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(filename string) error {
	f, err := os.Open(filename)
	if err != nil {
		return err
	}

	im, _, err := image.DecodeConfig(f)
	if err != nil {
		return err
	}
	fmt.Printf("%s\t%dx%d\n", filepath.Base(filename), im.Width, im.Height)
	return nil
}
