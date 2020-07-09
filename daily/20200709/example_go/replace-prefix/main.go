package main

import (
	"fmt"
	"go/ast"
	"go/printer"
	"go/token"
	"io"
	"log"
	"os"
	"strconv"
	"strings"

	"github.com/pkg/errors"
	"golang.org/x/tools/go/ast/astutil"
	"golang.org/x/tools/go/packages"
)

func main() {
	if err := run(os.Args[1:]); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run(args []string) error {
	path := args[0]
	pat := args[1] // github.com/<>/<>/%s
	rep := args[2] // "endpoint"
	overwrite := true

	fset := token.NewFileSet()
	cfg := &packages.Config{
		Fset: fset,
		Mode: packages.NeedName | packages.NeedSyntax,
	}
	pkgs, err := packages.Load(cfg, path)
	if err != nil {
		return errors.Wrap(err, "load")
	}
	if len(pkgs) < 1 {
		return errors.Wrap(fmt.Errorf("something wrong: must be len(pkgs) == 1, but %d", len(pkgs)), "not found")
	}

	pkg := pkgs[0]
	for _, f := range pkg.Syntax {
		prefix := ""
		// rewrite import path
		f := astutil.Apply(f, func(cur *astutil.Cursor) bool {
			node := cur.Node()
			if ispec, ok := node.(*ast.ImportSpec); ok {
				path, err := strconv.Unquote(ispec.Path.Value)
				if err != nil {
					panic(err) // xxx
				}

				if !strings.HasPrefix(path, pat) {
					return false
				}
				if ispec.Name != nil {
					prefix = ispec.Name.Name
				} else {
					xs := strings.Split(path, "/")
					prefix = xs[len(xs)-1]
				}
				ispec.Name = &ast.Ident{
					NamePos: ispec.Pos(),
					Name:    rep,
				}
				return false
			}
			return true
		}, nil)

		// rewrite body
		f = astutil.Apply(f, func(cur *astutil.Cursor) bool {
			node := cur.Node()
			if ident, ok := node.(*ast.Ident); ok {
				if ident.Name == prefix {
					ident.Name = rep
					return false
				}
			}
			return true
		}, nil)

		// emit
		filename := fset.File(f.Pos()).Name()
		var writer io.Writer
		writer = os.Stdout
		if overwrite {
			w, err := os.Create(filename)
			if err != nil {
				return err
			}
			writer = w
			defer w.Close()
		}
		if err := printer.Fprint(writer, fset, f); err != nil {
			return err
		}
	}
	return nil
}
