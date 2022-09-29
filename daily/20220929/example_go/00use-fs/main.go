package main

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
)

func main() {
	dirpath := os.Args[1]

	fs.WalkDir(os.DirFS(dirpath), ".", func(path string, d fs.DirEntry, err error) error {
		fmt.Println(path)
		return nil
	})

	fmt.Println("")
	fmt.Println("----------------------------------------")
	fmt.Println("")

	fs.WalkDir(os.DirFS(dirpath), ".", func(path string, d fs.DirEntry, err error) error {
		fmt.Println(strings.Repeat("#", len(strings.Split(path, string(os.PathSeparator)))), filepath.Base(path))
		return nil
	})
}
