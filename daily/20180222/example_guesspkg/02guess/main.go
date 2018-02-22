package main

import (
	"fmt"
	"go/build"
	"os"
	"os/user"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"
)

func guessPkg(path string) (string, error) {
	if strings.HasPrefix(path, "~") {
		u, err := user.Current()
		if err != nil {
			return "", err
		}
		path = filepath.Join(u.HomeDir, path[1:])
	}

	ctxt := build.Default
	path, err := filepath.Abs(path)
	if err != nil {
		return "", err
	}
	for _, srcdir := range ctxt.SrcDirs() {
		if strings.HasPrefix(path, srcdir) {
			pkgname := strings.TrimLeft(strings.Replace(path, srcdir, "", 1), "/")
			return pkgname, nil
		}
	}
	return "", errors.Errorf("%q is not subdir of srcdirs(%q)", path, build.Default.SrcDirs())
}

func guessPath(pkgname string) (string, error) {
	ctxt := build.Default
	for _, srcdir := range ctxt.SrcDirs() {
		path := filepath.Join(srcdir, pkgname)
		if info, err := os.Stat(path); err == nil && info.IsDir() {
			u, err := user.Current()
			if err != nil {
				return "", err
			}
			return strings.Replace(path, u.HomeDir, "~", 1), nil
		}
	}
	return "", errors.Errorf("%q's physical address is not found", pkgname)
}

func main() {
	{
		pkg := "golang.org/x/tools/go/loader"
		fmt.Println(guessPath(pkg))
		path, _ := guessPath(pkg)
		fmt.Println(guessPkg(path))
	}
	fmt.Println("----------------------------------------")
	{
		pkg := "encoding/json"
		fmt.Println(guessPath(pkg))
		path, _ := guessPath(pkg)
		fmt.Println(guessPkg(path))
	}
}
