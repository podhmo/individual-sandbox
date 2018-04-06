package main

import (
	"go/build"
	"io/ioutil"
	"log"
	"os"
	"sync"
	"time"

	"golang.org/x/tools/go/loader"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
	if err := run2WithCache(); err != nil {
		log.Fatalf("%+v", err)
	}
	if err := run2(); err != nil {
		log.Fatalf("%+v", err)
	}
	if err := run10WithCache(); err != nil {
		log.Fatalf("%+v", err)
	}
	if err := run10(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func newLoader(callback func(path string)) *loader.Config {
	ctxt := build.Default
	ctxt.ReadDir = func(path string) ([]os.FileInfo, error) {
		callback(path)
		return ioutil.ReadDir(path)
	}

	c := &loader.Config{
		TypeCheckFuncBodies: func(path string) bool {
			return false
		},
		Build: &ctxt,
	}
	c.TypeChecker.DisableUnusedImportCheck = true
	return c
}

func run() error {
	st := time.Now()
	defer func() {
		log.Println("time", time.Now().Sub(st))
	}()

	i := 0
	c := newLoader(func(path string) { i++ })

	c.Import("golang.org/x/tools/go/loader")
	prog, err := c.Load()
	if err != nil {
		return err
	}
	log.Println("load directory", "loaded", i, "allpackages", len(prog.AllPackages))

	return nil
}

func run2() error {
	st := time.Now()
	defer func() {
		log.Println("time", time.Now().Sub(st))
	}()

	i := 0
	{
		c := newLoader(func(path string) { i++ })
		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("golang.org/x/tools/go/loader")
		prog, err := c.Load()
		if err != nil {
			return err
		}
		log.Println("load directory2", "loaded", i, "allpackages", len(prog.AllPackages))
	}

	return nil
}

func run2WithCache() error {
	st := time.Now()
	defer func() {
		log.Println("time", time.Now().Sub(st))
	}()

	i := 0

	var m sync.Map
	findPackage := func(ctxt *build.Context, importPath, fromDir string, mode build.ImportMode) (*build.Package, error) {
		if build.IsLocalImport(importPath) {
			return ctxt.Import(importPath, fromDir, mode)
		}
		if pkg, ok := m.Load(importPath); ok {
			return pkg.(*build.Package), nil
		}
		pkg, err := ctxt.Import(importPath, fromDir, mode)
		if err != nil {
			return nil, err
		}
		m.Store(importPath, pkg)
		return pkg, nil
	}

	{
		c := newLoader(func(path string) { i++ })
		c.FindPackage = findPackage

		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}

	{
		c := newLoader(func(path string) { i++ })
		c.FindPackage = findPackage

		c.Import("golang.org/x/tools/go/loader")
		prog, err := c.Load()
		if err != nil {
			return err
		}
		log.Println("load directory2(with cache)", "loaded", i, "allpackages", len(prog.AllPackages))
	}
	return nil
}

func run10() error {
	st := time.Now()
	defer func() {
		log.Println("time", time.Now().Sub(st))
	}()

	i := 0
	{
		c := newLoader(func(path string) { i++ })
		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("golang.org/x/tools/go/loader")
		prog, err := c.Load()
		if err != nil {
			return err
		}
		log.Println("load directory10", "loaded", i, "allpackages", len(prog.AllPackages))
	}

	return nil
}

func run10WithCache() error {
	st := time.Now()
	defer func() {
		log.Println("time", time.Now().Sub(st))
	}()

	i := 0

	var m sync.Map
	findPackage := func(ctxt *build.Context, importPath, fromDir string, mode build.ImportMode) (*build.Package, error) {
		if build.IsLocalImport(importPath) {
			return ctxt.Import(importPath, fromDir, mode)
		}
		if pkg, ok := m.Load(importPath); ok {
			return pkg.(*build.Package), nil
		}
		pkg, err := ctxt.Import(importPath, fromDir, mode)
		if err != nil {
			return nil, err
		}
		m.Store(importPath, pkg)
		return pkg, nil
	}

	{
		c := newLoader(func(path string) { i++ })
		c.FindPackage = findPackage

		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}

	{
		c := newLoader(func(path string) { i++ })
		c.FindPackage = findPackage

		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.FindPackage = findPackage

		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}

	{
		c := newLoader(func(path string) { i++ })
		c.FindPackage = findPackage

		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.FindPackage = findPackage

		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}

	{
		c := newLoader(func(path string) { i++ })
		c.FindPackage = findPackage

		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.FindPackage = findPackage

		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}

	{
		c := newLoader(func(path string) { i++ })
		c.FindPackage = findPackage

		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.FindPackage = findPackage

		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
	}

	{
		c := newLoader(func(path string) { i++ })
		c.FindPackage = findPackage

		c.Import("golang.org/x/tools/go/loader")
		if _, err := c.Load(); err != nil {
			return err
		}
		c.Import("golang.org/x/tools/go/loader")
		prog, err := c.Load()
		if err != nil {
			return err
		}
		log.Println("load directory10 (with cache)", "loaded", i, "allpackages", len(prog.AllPackages))

	}
	return nil
}
