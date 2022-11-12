package main

import (
	"flag"
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"
	"strconv"
	"strings"
)

// go run main.go -replace $(git grep -l -P 'errors\.Wrapf?\(')

func main() {
	config := Config{}

	log.SetFlags(0)
	log.SetPrefix("log::")

	flag.BoolVar(&config.debug, "debug", config.debug, "show debug log")
	flag.BoolVar(&config.replace, "replace", config.replace, "write file instead of output to stdout")
	flag.BoolVar(&config.quiet, "quiet", config.quiet, "don't printer.Fprint(os.Stdout, fset, tree)")
	flag.StringVar(&config.tmpdir, "tmpdir", ".", "tmpdir")
	flag.VisitAll(func(f *flag.Flag) {
		if v := os.Getenv(strings.ToUpper(f.Name)); v != "" {
			f.Value.Set(v)
		}
	})
	flag.Parse()
	if err := run(config, flag.Args()); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Config struct {
	debug   bool
	replace bool
	quiet   bool
	tmpdir  string
}

func run(config Config, files []string) error {
	fset := token.NewFileSet()
	scanner := &Scanner{fset: fset, Config: config}
	fixer := &Fixer{fset: fset, Config: config}

	for _, filename := range files {
		if strings.HasPrefix(filename, "-") {
			continue
		}

		f, err := parser.ParseFile(fset, filename, nil, parser.ParseComments)
		if err != nil {
			return err
		}
		scanner.Scan(f)
	}

	for _, t := range scanner.targets {
		filename := fset.File(t.syntax.Pos()).Name()
		if !t.needFix {
			if config.debug {
				log.Printf("%-6s %s", "skip:", filename)
			}
			continue
		}
		fixer.Fix(t)

		// TODO: use goimports?

		if config.replace {
			wf, err := os.CreateTemp(config.tmpdir, "*.go")
			log.Printf("%-6s %s", "write:", filename)
			if err != nil {
				return fmt.Errorf("create file: %s, -- %w", filename, err)
			}
			if err := printer.Fprint(wf, fset, t.syntax); err != nil {
				return fmt.Errorf("emit file: %s, -- %w", filename, err)
			}
			if err := wf.Close(); err != nil { // xxx: use defer?
				return fmt.Errorf("emit file..: %s, -- %w", filename, err)
			}
			if err := os.Rename(wf.Name(), filename); err != nil {
				return fmt.Errorf("create file..: %s, -- %w", filename, err)
			}
		} else if !config.quiet {
			if err := printer.Fprint(os.Stdout, fset, t.syntax); err != nil {
				return fmt.Errorf("emit file: %s, -- %w", filename, err)
			}
		}
	}
	return nil
}

type Scanner struct {
	Config

	fset    *token.FileSet
	targets []*Target
}

func (s *Scanner) Scan(f *ast.File) {
	fset := s.fset
	if s.debug {
		log.Printf("%-6s %s", "scan:", fset.File(f.Pos()).Name())
	}
	// ast.Fprint(os.Stdout, fset, f, nil)

	target := &Target{syntax: f}
	var stack []ast.Node
	ast.Inspect(f, func(n ast.Node) bool {
		if n == nil {
			stack = stack[:len(stack)-1] // pop
		} else {
			stack = append(stack, n) // push
		}

		switch n := n.(type) {
		case *ast.CallExpr:
			// errors.Wrap() or errors.Wrapf()
			if fun, ok := n.Fun.(*ast.SelectorExpr); ok {
				if prefix, ok := fun.X.(*ast.Ident); ok && prefix.Name == "errors" {
					if s.debug {
						log.Printf("\t%-6s %10s():%d", "scan:", fun.Sel.Name, fset.File(f.Pos()).Line(n.Pos()))
					}

					switch fun.Sel.Name {
					case "Wrap", "Wrapf":
						if ret, ok := stack[len(stack)-2].(*ast.ReturnStmt); ok {
							set := func(new *ast.CallExpr) {
								for i, x := range ret.Results {
									if x == n {
										ret.Results[i] = new
									}
								}
							}
							target.calls = append(target.calls, &call{name: fun.Sel.Name, expr: n, stack: stack[:], set: set})
						}
					}
				}
			}
		}
		return true
	})
	target.needFix = len(target.calls) > 0
	s.targets = append(s.targets, target)
}

type Fixer struct {
	Config

	fset *token.FileSet
}

func (f *Fixer) Fix(target *Target) {
	fset := f.fset
	syntax := target.syntax

	log.Printf("%-6s %s", "fix:", fset.File(syntax.Pos()).Name())
	for _, call := range target.calls {
		pos := call.expr.Pos()
		if f.debug {
			log.Printf("\t%-6s %10s():%d", "fix:", call.name, fset.File(syntax.Pos()).Line(pos))
		}

		switch call.name {
		case "Wrap", "Wrapf": // errors.Wrap(err, "<...>") -> fmt.Errorf("<...> -- %w", err)
			errArg := call.expr.Args[0]
			fmtArg := call.expr.Args[1]

			if v, ok := fmtArg.(*ast.BasicLit); ok && v.Kind == token.STRING {
				fmtArg = &ast.BasicLit{ValuePos: v.Pos(), Kind: v.Kind, Value: strconv.Quote(strings.Trim(v.Value, "- \"") + " -- %w")}
			} else {
				fmtArg = &ast.BinaryExpr{X: v, OpPos: v.Pos(), Op: token.ADD, Y: &ast.BasicLit{ValuePos: v.Pos(), Kind: token.STRING, Value: " -- %w"}}
			}

			args := make([]ast.Expr, 0, len(call.expr.Args)+1)
			args = append(args, fmtArg)
			args = append(args, call.expr.Args[2:]...)
			args = append(args, errArg)
			call.set(&ast.CallExpr{
				Fun:    &ast.SelectorExpr{X: &ast.Ident{NamePos: pos, Name: "fmt"}, Sel: &ast.Ident{NamePos: pos, Name: "Errorf"}},
				Lparen: pos,
				Args:   args,
				Rparen: pos,
			})
		}
	}
}

type Target struct {
	syntax *ast.File
	calls  []*call

	needFix bool
}
type call struct {
	name string

	expr  *ast.CallExpr
	stack []ast.Node
	set   func(*ast.CallExpr)
}
