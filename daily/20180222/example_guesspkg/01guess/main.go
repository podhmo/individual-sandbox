package main

import (
	"fmt"
	"go/build"
	"os/user"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"
	"golang.org/x/tools/go/buildutil"
)

func guessPkg(ctxt *build.Context, path string) (string, error) {
	if strings.HasPrefix(path, "~") {
		u, err := user.Current()
		if err != nil {
			return "", err
		}
		path = filepath.Join(u.HomeDir, path[1:])
	}

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

func guessPath(ctxt *build.Context, pkgname string) (string, error) {
	for _, srcdir := range ctxt.SrcDirs() {
		path := buildutil.JoinPath(ctxt, srcdir, pkgname)
		if buildutil.IsDir(ctxt, path) {
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
	ctxt := &build.Default
	{
		pkg := "golang.org/x/tools/go/loader"
		fmt.Println(guessPath(ctxt, pkg))
		path, _ := guessPath(ctxt, pkg)
		fmt.Println(guessPkg(ctxt, path))
	}
	fmt.Println("----------------------------------------")
	{
		pkg := "encoding/json"
		fmt.Println(guessPath(ctxt, pkg))
		path, _ := guessPath(ctxt, pkg)
		fmt.Println(guessPkg(ctxt, path))
	}
}
