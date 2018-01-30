package main

import (
	"flag"
	"fmt"
	"go/ast"
	"go/build"
	"go/parser"
	"go/token"
	"go/types"
	"io/ioutil"
	"os"
	"path/filepath"
	"sync"
	"time"

	"github.com/k0kubun/pp"
)

var (
	// main operation modes
	testFiles  = flag.Bool("t", false, "include in-package test files in a directory")
	xtestFiles = flag.Bool("x", false, "consider only external test files in a directory")
	allErrors  = flag.Bool("e", false, "report all errors, not just the first 10")
	verbose    = flag.Bool("v", false, "verbose mode")
	compiler   = flag.String("c", "source", "compiler used for installed packages (gc, gccgo, or source)")

	// additional output control
	printAST      = flag.Bool("ast", false, "print AST (forces -seq)")
	printTrace    = flag.Bool("trace", false, "print parse trace (forces -seq)")
	parseComments = flag.Bool("comments", false, "parse comments (ignored unless -ast or -trace is provided)")
)

var (
	fset       = token.NewFileSet()
	errorCount = 0
	sequential = false
	parserMode parser.Mode
)

func initParserMode() {
	if *allErrors {
		parserMode |= parser.AllErrors
	}
	if *printAST {
		sequential = true
	}
	if *printTrace {
		parserMode |= parser.Trace
		sequential = true
	}
	if *parseComments && (*printAST || *printTrace) {
		parserMode |= parser.ParseComments
	}
}

const usageString = `usage: parse [flags] [path ...]
`

func usage() {
	fmt.Fprintln(os.Stderr, usageString)
	flag.PrintDefaults()
	os.Exit(2)
}

// parse may be called concurrently
func parse(filename string, src interface{}) (*ast.File, error) {
	if *verbose {
		fmt.Println(filename)
	}
	file, err := parser.ParseFile(fset, filename, src, parserMode) // ok to access fset concurrently
	if *printAST {
		ast.Print(fset, file)
	}
	return file, err
}

func parseStdin() (*ast.File, error) {
	src, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		return nil, err
	}
	return parse("<standard input>", src)
}

func parseFiles(dir string, filenames []string) ([]*ast.File, error) {
	files := make([]*ast.File, len(filenames))
	errors := make([]error, len(filenames))

	var wg sync.WaitGroup
	for i, filename := range filenames {
		wg.Add(1)
		go func(i int, filepath string) {
			defer wg.Done()
			files[i], errors[i] = parse(filepath, nil)
		}(i, filepath.Join(dir, filename))
		if sequential {
			wg.Wait()
		}
	}
	wg.Wait()

	// if there are errors, return the first one for deterministic results
	for _, err := range errors {
		if err != nil {
			return nil, err
		}
	}

	return files, nil
}

func parseDir(dir string) ([]*ast.File, error) {
	ctxt := build.Default
	pkginfo, err := ctxt.ImportDir(dir, 0)
	if _, nogo := err.(*build.NoGoError); err != nil && !nogo {
		return nil, err
	}

	if *xtestFiles {
		return parseFiles(dir, pkginfo.XTestGoFiles)
	}

	filenames := append(pkginfo.GoFiles, pkginfo.CgoFiles...)
	if *testFiles {
		filenames = append(filenames, pkginfo.TestGoFiles...)
	}
	return parseFiles(dir, filenames)
}

func getPkgFiles(args []string) ([]*ast.File, error) {
	if len(args) == 0 {
		// stdin
		file, err := parseStdin()
		if err != nil {
			return nil, err
		}
		return []*ast.File{file}, nil
	}

	if len(args) == 1 {
		// possibly a directory
		path := args[0]
		info, err := os.Stat(path)
		if err != nil {
			return nil, err
		}
		if info.IsDir() {
			return parseDir(path)
		}
	}

	// list of files
	return parseFiles("", args)
}

func main() {
	flag.Usage = usage
	flag.Parse()
	initParserMode()

	start := time.Now()
	files, err := getPkgFiles(flag.Args())
	_ = err

	pp.Println(types.Eval(fset, files[0].Package, token.NoPos, "Person{}"))
}
