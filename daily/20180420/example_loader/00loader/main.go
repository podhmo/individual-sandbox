package main

import (
	"fmt"
	"go/ast"
	"go/build"
	"go/parser"
	"go/token"
	"go/types"
	"log"
	"os"

	"golang.org/x/tools/go/ast/astutil"
)

// Config :
type Config struct {
	Fset        *token.FileSet
	ParserMode  parser.Mode
	Build       *build.Context
	Cwd         string
	DisplayPath func(path string) string
	AllowErrors bool
	CreatePkgs  []PkgSpec
	ImportPkgs  map[string]bool
	FindPackage func(ctxt *build.Context, importPath, fromDir string, mode build.ImportMode) (*build.Package, error)
}

func (conf *Config) fset() *token.FileSet {
	if conf.Fset == nil {
		conf.Fset = token.NewFileSet()
	}
	return conf.Fset
}

// ParseFile :
func (conf *Config) ParseFile(filename string, src interface{}) (*ast.File, error) {
	// TODO(adonovan): use conf.build() etc like parseFiles does.
	return parser.ParseFile(conf.fset(), filename, src, conf.ParserMode)
}

// CreateFromFilenames :
func (conf *Config) CreateFromFilenames(path string, filenames ...string) {
	conf.CreatePkgs = append(conf.CreatePkgs, PkgSpec{Path: path, Filenames: filenames})
}

// CreateFromFiles :
func (conf *Config) CreateFromFiles(path string, files ...*ast.File) {
	conf.CreatePkgs = append(conf.CreatePkgs, PkgSpec{Path: path, Files: files})
}

// ImportWithTests :
func (conf *Config) ImportWithTests(path string) { conf.addImport(path, true) }

// Import :
func (conf *Config) Import(path string) { conf.addImport(path, false) }

func (conf *Config) addImport(path string, tests bool) {
	if path == "C" {
		return // ignore; not a real package
	}
	if conf.ImportPkgs == nil {
		conf.ImportPkgs = make(map[string]bool)
	}
	conf.ImportPkgs[path] = conf.ImportPkgs[path] || tests
}

// PkgSpec :
type PkgSpec struct {
	Path      string      // package path ("" => use package declaration)
	Files     []*ast.File // ASTs of already-parsed files
	Filenames []string    // names of files to be parsed
}

// Program :
type Program struct {
	Fset        *token.FileSet // the file set for this program
	Created     []*PackageInfo
	Imported    map[string]*PackageInfo
	AllPackages map[*types.Package]*PackageInfo
}

// PathEnclosingInterval :
func (prog *Program) PathEnclosingInterval(start, end token.Pos) (pkg *PackageInfo, path []ast.Node, exact bool) {
	for _, info := range prog.AllPackages {
		for _, f := range info.Files {
			if f.Pos() == token.NoPos {
				// This can happen if the parser saw
				// too many errors and bailed out.
				// (Use parser.AllErrors to prevent that.)
				continue
			}
			if !tokenFileContainsPos(prog.Fset.File(f.Pos()), start) {
				continue
			}
			if path, exact := astutil.PathEnclosingInterval(f, start, end); path != nil {
				return info, path, exact
			}
		}
	}
	return nil, nil, false
}

func tokenFileContainsPos(f *token.File, pos token.Pos) bool {
	p := int(pos)
	base := f.Base()
	return base <= p && p < base+f.Size()
}

// InitialPackages returns a new slice containing the set of initial
// packages (Created + Imported) in unspecified order.
//
func (prog *Program) InitialPackages() []*PackageInfo {
	infos := make([]*PackageInfo, 0, len(prog.Created)+len(prog.Imported))
	infos = append(infos, prog.Created...)
	for _, info := range prog.Imported {
		infos = append(infos, info)
	}
	return infos
}

// Package returns the ASTs and results of type checking for the
// specified package.
func (prog *Program) Package(path string) *PackageInfo {
	if info, ok := prog.AllPackages[prog.importMap[path]]; ok {
		return info
	}
	for _, info := range prog.Created {
		if path == info.Pkg.Path() {
			return info
		}
	}
	return nil
}

// PackageInfo :
type PackageInfo struct {
	Path      string
	Files     []*ast.File
	Filenames []string
	Errors    []error
	errorFunc func(error)
}

func (info *PackageInfo) String() string { return info.Path }

func (info *PackageInfo) appendError(err error) {
	if info.errorFunc != nil {
		info.errorFunc(err)
	} else {
		fmt.Fprintln(os.Stderr, err)
	}
	info.Errors = append(info.Errors, err)
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	return nil
}
