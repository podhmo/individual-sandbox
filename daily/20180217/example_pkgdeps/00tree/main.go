package main

import (
	"fmt"
	"go/build"
	"go/parser"
	"go/types"
	"log"
	"os"
	"strings"

	"sort"

	"github.com/pkg/errors"
	"golang.org/x/tools/go/loader"
	kingpin "gopkg.in/alecthomas/kingpin.v2"
)

type opt struct {
	ignoreStdPkg      bool
	ignoreInternalPkg bool
	showID            bool
}
type s struct {
	prog    *loader.Program
	opt     *opt
	arrived map[string]int
}

func isStdPackage(s *s, pkg *types.Package) bool {
	files := s.prog.Package(pkg.Path()).Files
	if len(files) > 0 {
		filepath := s.prog.Fset.Position(files[0].Package).Filename
		return strings.HasPrefix(filepath, build.Default.GOROOT)
	}
	// fmt.Println("!!!", pkg.Path()) // xxx (e.g. unsafe)
	return true
}

func isInternalPackage(s *s, pkg *types.Package) bool {
	return strings.Contains(pkg.Path(), "/internal/")
}

func load(pkg string) (*loader.Program, error) {
	conf := loader.Config{
		ParserMode: parser.ImportsOnly,
		TypeCheckFuncBodies: func(path string) bool {
			return false
		},
	}
	conf.Import(pkg)

	prog, err := conf.Load()
	if err != nil {
		return nil, errors.Wrap(err, "load")
	}

	return prog, nil
}

func dump(pkg *types.Package, s *s, depth int) error {
	id, arrived := s.arrived[pkg.Path()]
	if !arrived {
		id = len(s.arrived)
		s.arrived[pkg.Path()] = id
	}
	if (!s.opt.ignoreStdPkg || !isStdPackage(s, pkg)) && (!s.opt.ignoreInternalPkg || !isInternalPackage(s, pkg)) {
		if s.opt.showID {
			fmt.Printf("%s%s #=%d\n", strings.Repeat("  ", depth), pkg.Path(), id)
		} else {
			fmt.Printf("%s%s\n", strings.Repeat("  ", depth), pkg.Path())
		}
	}
	if arrived {
		return nil
	}

	deps := pkg.Imports()
	sort.Slice(deps, func(i int, j int) bool {
		return deps[i].Name() < deps[j].Name()
	})
	for _, deppkg := range deps {
		dump(deppkg, s, depth+1)
	}
	return nil
}

func main() {
	app := kingpin.New("pkgtree", "dump pkg dependencies")
	pkg := app.Arg("pkg", "pkg").Required().String()
	ignoreStdPkg := app.Flag("ignore-std-pkg", "").Bool()
	ignoreInternalPkg := app.Flag("ignore-internal-pkg", "").Bool()
	showID := app.Flag("show-id", "").Bool()

	if _, err := app.Parse(os.Args[1:]); err != nil {
		app.FatalUsage(fmt.Sprintf("%v", err))
	}

	prog, err := load(*pkg)
	if err != nil {
		log.Fatalf("!!%+v", err)
	}
	opt := &opt{
		ignoreStdPkg:      *ignoreStdPkg,
		ignoreInternalPkg: *ignoreInternalPkg,
		showID:            *showID,
	}
	s := &s{
		arrived: map[string]int{},
		opt:     opt,
		prog:    prog,
	}
	if err := dump(prog.Package(*pkg).Pkg, s, 0); err != nil {
		log.Fatalf("!!%v", err)
	}
}
