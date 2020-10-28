package main

import (
	"fmt"
	"go/ast"
	"go/printer"
	"go/token"
	"io"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"

	"golang.org/x/tools/go/packages"
)

// TODO: support <pkg>
// TODO: support <pkg>.<type name>
// TODO: support <pkg>.<function name>
// TODO: support <pkg>.<type name>.<method name>
// TODO: handling unexported methods/functions
// TODO: support recursion
// TODO: support mutual recursion
// TODO: display not only name, also the signature of function
// Todo: support *ast.IndexExpr (type information is needed?)

func main() {
	if err := run(os.Args[1:]); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run(pattern []string) error {
	fset := token.NewFileSet()
	cfg := &packages.Config{
		Fset: fset,
		Mode: packages.NeedName | packages.NeedSyntax,
	}
	pkgs, err := packages.Load(cfg, pattern...)
	if err != nil {
		return fmt.Errorf("packages, load %w", err)
	}
	if len(pkgs) != 1 {
		return fmt.Errorf("unexpected packages count %d", len(pkgs))
	}

	pkg := pkgs[0]
	files := make(map[string]*ast.File, len(pkg.Syntax))
	for _, f := range pkg.Syntax {
		f := f
		tf := fset.File(f.Pos())
		files[tf.Name()] = f
	}
	p, err := NewPackageFromAST(fset, &ast.Package{
		Name:  pkg.PkgPath,
		Files: files,
	})
	if err != nil {
		return fmt.Errorf("new package from ast %w", err)
	}
	return NewPrinter(os.Stdout).Print(p)
}

func NewPackageFromAST(fset *token.FileSet, pkg *ast.Package) (*Package, error) {
	p := &Package{
		Name:  pkg.Name,
		fset:  fset,
		Files: map[string]*File{},
		Funcs: map[string]*Func{},
	}
	return p, p.parseFiles(pkg.Files)
}

func (p *Package) getName(node ast.Node, hist []ast.Node) string {
	switch t := node.(type) {
	case *ast.Ident:
		return t.Name
	case *ast.SelectorExpr:
		return p.getName(t.Sel, append(hist, node))
	default:
		if ok, _ := strconv.ParseBool(os.Getenv("DEBUG")); ok {
			printer.Fprint(os.Stderr, p.fset, hist[0])
			fmt.Fprintln(os.Stderr, "\n----------------------------------------")
		}
		log.Printf("unexpected type: %T", t)
		return ""
	}
}

type Package struct {
	Name  string           `json:"name"`
	Funcs map[string]*Func `json:"funcs"`
	Files map[string]*File `json:"-"`

	fset *token.FileSet
}

func (p *Package) parseFiles(files map[string]*ast.File) error {
	for _, ftree := range files {
		f := p.FileFromAST(ftree)
		for _, decl := range ftree.Decls {
			if decl, ok := decl.(*ast.FuncDecl); ok {
				// TODO: skip unexported
				// TODO: method
				// TODO: dedup
				fun := f.FuncFromAST(decl)
				ast.Inspect(decl.Body, func(node ast.Node) bool {
					if node, ok := node.(*ast.CallExpr); ok {
						switch ob := node.Fun.(type) {
						case *ast.Ident:
							if fun.Recv != "" { // method
								return true
							}
							if isBuiltin(ob.Name) {
								return true
							}
							fun.Calls = append(fun.Calls, p.FuncFromName(ob.Name))
						case *ast.SelectorExpr:
							if fun.Recv == "" { // function
								return true
							}
							// TODO: see type
							// TODO: support `func (*Type) Method () {}`
							if fun.Recv != p.getName(ob.X, []ast.Node{ob.X}) { // xxx
								return true
							}
							fun.Calls = append(fun.Calls, p.FuncFromName(ob.Sel.Name))
						case *ast.ArrayType:
							log.Printf("not supported type: %T", node.Fun)
							return true
						case *ast.FuncLit:
							log.Printf("not supported type: %T", node.Fun)
							return true
						case *ast.InterfaceType:
							log.Printf("not supported type: %T", node.Fun)
							return true
						default:
							if ok, _ := strconv.ParseBool(os.Getenv("DEBUG")); ok {
								fmt.Fprintln(os.Stderr, "----", fun, "---")
								printer.Fprint(os.Stderr, p.fset, node)
								fmt.Fprintln(os.Stderr, "\n----------------------------------------")
							}
							panic(fmt.Sprintf("unexpected type: %T", node.Fun))
						}
					}
					return true
				})
			}
		}
	}
	return nil
}

func (p *Package) FuncFromName(name string) *Func {
	f, ok := p.Funcs[name]
	if ok {
		return f
	}
	f = &Func{Name: name, Calls: []*Func{}, Kind: FuncKindFunction}
	p.Funcs[name] = f
	return f
}
func (p *Package) FileFromAST(node *ast.File) *File {
	tf := p.fset.File(node.Pos())
	name := tf.Name()
	f, ok := p.Files[name]
	if ok {
		return f
	}
	f = &File{Name: name, Funcs: map[string][]*Func{}, node: node, pkg: p}
	p.Files[name] = f
	return f
}

type File struct {
	Name  string
	Funcs map[string][]*Func `json:"-"`

	pkg  *Package
	node *ast.File
}

func (f *File) FuncFromAST(decl *ast.FuncDecl) *Func {
	name := decl.Name.Name
	tf := f.pkg.fset.File(f.node.Pos())

	fun := f.pkg.FuncFromName(name)
	f.Funcs[name] = append(f.Funcs[name], fun)

	fun.node = decl
	fun.File = tf.Name()
	fun.Line = tf.Line(decl.Pos())
	if decl.Recv != nil {
		fun.Kind = FuncKindMethod
		if len(decl.Recv.List[0].Names) > 0 {
			fun.Recv = decl.Recv.List[0].Names[0].Name // xxx
		}
	}
	return fun
}

type FuncKind string

const (
	FuncKindFunction FuncKind = "F"
	FuncKindMethod            = "M"
)

type Func struct {
	Name  string   `json:"name"`
	File  string   `json:"file"`
	Line  int      `json:"line"`
	Recv  string   `json:"-"`
	Kind  FuncKind `json:"kind"`
	Calls []*Func  `json:"calls"`

	pkg  *Package
	node ast.Node
}

func (f *Func) String() string {
	return fmt.Sprintf("%s.%s:%d", f.File, f.Name, f.Line)
}

// printer
type Printer struct {
	w      io.Writer
	lv     int
	indent string
}

func (p *Printer) Print(ob interface{}) error {
	switch ob := ob.(type) {
	case *Package:
		return p.PrintPackage(ob)
	case *Func:
		return p.PrintFunc(ob)
	default:
		return fmt.Errorf("unexpected type: %T, support only *Package or *Func", ob)
	}
}

func (p *Printer) PrintPackage(pkg *Package) error {
	fmt.Fprintf(p.w, "%s\n", pkg.Name)
	p.lv++
	defer func() { p.lv-- }()

	roots := make([]*Func, 0, len(pkg.Funcs))
	inner := map[*Func]int{}
	for _, f := range pkg.Funcs {
		f := f
		if len(f.Calls) > 0 {
			roots = append(roots, f)
			for _, sf := range f.Calls {
				if sf == f {
					continue // recursive
				}
				inner[sf]++
			}
		}
	}
	sort.Slice(roots, func(i, j int) bool { return roots[i].node.Pos() < roots[j].node.Pos() })

	seen := map[*Func]int{}
	for _, f := range roots {
		if _, notOuter := inner[f]; notOuter {
			continue
		}
		if err := p.printFunc(f, seen); err != nil {
			return err
		}
	}
	return nil
}

func (p *Printer) PrintFunc(f *Func) error {
	seen := map[*Func]int{}
	p.lv++
	defer func() { p.lv-- }()
	return p.printFunc(f, seen)
}

func (p *Printer) printFunc(f *Func, seen map[*Func]int) error {
	if id, ok := seen[f]; ok {
		if p.lv <= 1 {
			return nil
		}
		fmt.Fprintf(p.w, "%s%s:%s() #=%d\n", strings.Repeat(p.indent, p.lv), f.Kind, f.Name, id)
		return nil
	}
	seen[f] = len(seen)
	if f.Kind == FuncKindFunction {
		fmt.Fprintf(p.w, "%s%s:%s()\n", strings.Repeat(p.indent, p.lv), f.Kind, f.Name)
	} else {
		fmt.Fprintf(p.w, "%s%s:%s.%s()\n", strings.Repeat(p.indent, p.lv), f.Kind, f.Recv, f.Name)
	}
	p.lv++
	defer func() { p.lv-- }()

	// sort
	children := make([]*Func, 0, len(f.Calls))
	inner := map[*Func]bool{}
	for _, child := range f.Calls {
		child := child
		if _, ok := inner[child]; ok {
			continue
		}
		inner[child] = true
		if child.node == nil {
			if ok, _ := strconv.ParseBool(os.Getenv("DEBUG")); ok {
				log.Printf("unexpected node: %s", child)
			}
			continue
		}
		children = append(children, child)
	}
	sort.Slice(children, func(i, j int) bool { return children[i].node.Pos() < children[j].node.Pos() })

	for _, child := range children {
		if err := p.printFunc(child, seen); err != nil {
			return fmt.Errorf("%s: %w", f.Name, err)
		}
	}
	return nil
}

func NewPrinter(w io.Writer) *Printer {
	return &Printer{
		w:      w,
		indent: "  ",
	}
}

// ----------------------------------------
// helpers
// ----------------------------------------
func isBuiltin(name string) bool {
	for _, s := range builtins {
		if name == s {
			return true
		}
	}
	return false
}

var builtins = []string{
	"ComplexType",
	"FloatType",
	"IntegerType",
	"Type",
	"Type1",
	"append",
	"bool",
	"byte",
	"cap",
	"close",
	"complex",
	"complex128",
	"complex64",
	"copy",
	"delete",
	"error",
	"false",
	"float32",
	"float64",
	"imag",
	"int",
	"int16",
	"int32",
	"int64",
	"int8",
	"iota",
	"len",
	"make",
	"new",
	"nil",
	"panic",
	"print",
	"println",
	"real",
	"recover",
	"rune",
	"string",
	"swap",
	"true",
	"uint",
	"uint16",
	"uint32",
	"uint64",
	"uint8",
	"uintptr",
}
