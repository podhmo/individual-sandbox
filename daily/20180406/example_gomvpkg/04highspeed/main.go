package main

import (
	"fmt"
	"go/ast"
	"go/build"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"log"
	"strconv"
	"strings"

	"github.com/k0kubun/pp"
	"golang.org/x/tools/go/buildutil"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	ctxt := &build.Default
	inpkg := "github.com/podhmo/handwriting"
	srcpkg := "golang.org/x/tools/go/loader"
	// dstpkg := "github.com/xxx/loader"

	// find root
	var rootdir string
	for _, dir := range ctxt.SrcDirs() {
		path := buildutil.JoinPath(ctxt, dir, inpkg)
		if buildutil.IsDir(ctxt, path) {
			rootdir = path
			log.Println("found root", path)
			break
		}
	}

	// find go package in inpkg
	var pkgdirs []string
	q := []string{rootdir}

	for len(q) > 0 {
		dir := q[0]
		q = q[1:]

		fs, err := buildutil.ReadDir(ctxt, dir)
		if err != nil {
			return err
		}

		used := false
		for _, f := range fs {
			if f.IsDir() {
				q = append(q, buildutil.JoinPath(ctxt, dir, f.Name()))
				continue
			}
			if used {
				continue
			}
			if strings.HasSuffix(f.Name(), ".go") {
				log.Println("found ", dir)
				pkgdirs = append(pkgdirs, dir)
				used = true
			}
		}
	}
	type Affected struct {
		dir     string
		name    string
		pkgpath string
		files   []string
	}

	var affected []Affected

	// find affected pkg
	fset := token.NewFileSet()
	for _, dir := range pkgdirs {
		fs, err := buildutil.ReadDir(ctxt, dir)
		if err != nil {
			return err
		}

		pkgpath := dir
		for _, prefix := range ctxt.SrcDirs() {
			if strings.HasPrefix(pkgpath, prefix) {
				pkgpath = pkgpath[len(prefix)+1:]
				break
			}
		}
		current := Affected{dir: dir, pkgpath: pkgpath}
		for _, f := range fs {
			if !strings.HasSuffix(f.Name(), ".go") {
				continue
			}
			func() {
				r, err := buildutil.OpenFile(ctxt, buildutil.JoinPath(ctxt, dir, f.Name()))
				if err != nil {
					log.Println(f.Name(), err)
					return
				}
				defer r.Close()
				astf, err := parser.ParseFile(fset, f.Name(), r, parser.ImportsOnly)
				if err != nil {
					log.Println(f.Name(), err)
					return
				}

				current.name = astf.Name.Name

				for _, is := range astf.Imports {
					path, err := strconv.Unquote(is.Path.Value)
					if err != nil {
						log.Println(f.Name(), err)
					}
					if path == srcpkg {
						current.files = append(current.files, f.Name())
						break
					}
				}
			}()
		}
		if len(current.files) > 0 {
			affected = append(affected, current)
			log.Println("affected", affected)
		}
	}

	fset = token.NewFileSet()
	c := types.Config{
		Importer:                 &Importer{Importer: importer.Default().(types.ImporterFrom)},
		DisableUnusedImportCheck: true,
	}
	var pkgs []*types.Package

	for _, a := range affected {
		fmt.Println(types.NewPackage(a.pkgpath, a.name))
		files := make([]*ast.File, 0, len(a.files))
		for _, filename := range a.files {
			func() {
				r, err := buildutil.OpenFile(ctxt, buildutil.JoinPath(ctxt, a.dir, filename))
				if err != nil {
					log.Println(err)
					return
				}
				defer r.Close()
				astf, err := parser.ParseFile(fset, filename, r, parser.ParseComments)
				if err != nil {
					log.Println(err)
					return
				}
				files = append(files, astf)
			}()
		}
		info := types.Info{
			Defs: map[*ast.Ident]types.Object{},
			Uses: map[*ast.Ident]types.Object{},
		}
		pkg, err := c.Check(a.pkgpath, fset, files, &info)
		if err != nil {
			return err
		}
		pkgs = append(pkgs, pkg)
	}
	fmt.Println(len(pkgs))
	return nil
}

type Importer struct {
	Importer types.ImporterFrom
}

func (i *Importer) Import(path string) (*types.Package, error) {
	pkg, err := i.Importer.Import(path)
	if err != nil {
		pp.Println("import", err)
		elems := strings.Split(path, "/")
		return types.NewPackage(path, elems[len(elems)-1]), nil
	}
	return pkg, nil
}

func (i *Importer) ImportFrom(path, dir string, mode types.ImportMode) (*types.Package, error) {
	pkg, err := i.Importer.ImportFrom(path, dir, mode)
	if err != nil {
		pp.Println("importfrom", path, dir, mode, err)
		//elems := strings.Split(path, "/")
		return types.NewPackage(path, "reporting"), nil
		//		return types.NewPackage(path, elems[len(elems)-1]), nil
	}
	return pkg, nil
}
