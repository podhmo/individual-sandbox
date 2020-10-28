package main

import (
	"fmt"
	"go/ast"
	"go/printer"
	"go/token"
	"go/types"
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
// TODO: support *ast.IndexExpr (type information is needed?)
// TODO: support on method, include other types's method callings if same package.
// TODO: grouping by struct

func main() {
	// mode := ModeIgnoreUnexported
	var mode InspectMode
	mode = mode | ModeSelfOnly
	inspector := &Inspector{Mode: mode}
	if err := inspector.Inspect(os.Args[1:]); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type InspectMode int

const (
	ModeExportedOnly InspectMode = 1 << iota
	ModeSelfOnly
)

type Inspector struct {
	Mode InspectMode
}

func (i *Inspector) Inspect(pattern []string) error {
	fset := token.NewFileSet()
	cfg := &packages.Config{
		Fset: fset,
		Mode: packages.NeedName | packages.NeedSyntax | packages.NeedTypes | packages.NeedTypesInfo,
	}
	pkgs, err := packages.Load(cfg, pattern...)
	if err != nil {
		return fmt.Errorf("packages, load %w", err)
	}
	if len(pkgs) != 1 {
		return fmt.Errorf("unexpected packages count %d", len(pkgs))
	}

	pkg := pkgs[0]
	p := &Package{
		Name:  pkg.PkgPath,
		Files: map[string]*File{},
		Funcs: map[string]*Func{},
		fset:  fset,
		pkg:   pkg,
		mode:  i.Mode,
	}
	files := make(map[string]*ast.File, len(pkg.Syntax))
	for _, f := range pkg.Syntax {
		f := f
		tf := fset.File(f.Pos())
		files[tf.Name()] = f
	}
	if err := p.parseFiles(files); err != nil {
		return fmt.Errorf("new package from ast %w", err)
	}

	printer := &Printer{
		w:      os.Stdout,
		indent: "  ",
		info:   pkg.TypesInfo,
		mode:   i.Mode,
	}
	return printer.Print(p)
}

type Package struct {
	Name  string           `json:"name"`
	Funcs map[string]*Func `json:"funcs"`
	Files map[string]*File `json:"-"`

	fset *token.FileSet
	pkg  *packages.Package
	mode InspectMode
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

func (p *Package) parseFiles(files map[string]*ast.File) error {
	for _, ftree := range files {
		f := p.FileFromAST(ftree)
		for _, decl := range ftree.Decls {
			if decl, ok := decl.(*ast.FuncDecl); ok {
				fun := f.FuncFromAST(decl)
				target := p.pkg.TypesInfo.ObjectOf(decl.Name)
				ast.Inspect(decl.Body, func(node ast.Node) bool {
					if node, ok := node.(*ast.CallExpr); ok {
						switch t := node.Fun.(type) {
						case *ast.Ident:
							if p.mode&ModeSelfOnly != 0 {
								return true
							}
							ob := p.pkg.TypesInfo.ObjectOf(t)
							if target.Pkg() != ob.Pkg() {
								return true
							}
							fun.Calls = append(fun.Calls, p.FuncFromName(t.Name))
						case *ast.SelectorExpr:
							ob := p.pkg.TypesInfo.ObjectOf(t.Sel)
							if target.Pkg() != ob.Pkg() {
								return true
							}
							if p.mode&ModeSelfOnly != 0 {
								if decl.Recv == nil {
									return true
								}
								switch t := ob.Type().(type) {

								case *types.Signature:
									if t.Recv() == nil {
										return true
									}
									if t.Recv() != nil && t.Recv().String() != target.Type().(*types.Signature).Recv().String() {
										return true
									}
								case *types.Named:
									// skip
									return true
								default:
									panic(fmt.Sprintf("unexpected recv type %T", t))
								}
							}
							// TODO: only self. どこで？が消えている?
							fun.Calls = append(fun.Calls, p.FuncFromName(t.Sel.Name))
						case *ast.ArrayType:
							log.Printf("not supported type: %T", node.Fun)
							return true
						case *ast.FuncLit:
							log.Printf("not supported type: %T", node.Fun)
							return true
						case *ast.InterfaceType:
							log.Printf("not supported type: %T", node.Fun)
							return true
						case *ast.ParenExpr:
							log.Printf("not supported type: %T", node.Fun)
							return true
						case *ast.CallExpr:
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
	f = &Func{Name: name, Calls: []*Func{}, Kind: FuncKindFunction, pkg: p}
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

	info *types.Info
	mode InspectMode
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
		if p.mode&ModeExportedOnly != 0 && f.Name[0] == strings.ToLower(f.Name)[0] {
			continue
		}
		if err := p.printFunc(f, seen); err != nil {
			return err
		}
		fmt.Fprintln(p.w, "")
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
		if p.mode&ModeExportedOnly != 0 && f.Name[0] == strings.ToLower(f.Name)[0] {
			return nil
		}
		fmt.Fprintf(p.w, "%s%s #=%d\n", strings.Repeat(p.indent, p.lv), strings.ReplaceAll(p.info.ObjectOf(f.node.(*ast.FuncDecl).Name).String(), f.pkg.Name+".", ""), id)
		return nil
	}

	seen[f] = len(seen)
	if p.mode&ModeExportedOnly != 0 && f.Name[0] == strings.ToLower(f.Name)[0] {
		return nil
	}
	fmt.Fprintf(p.w, "%s%s\n", strings.Repeat(p.indent, p.lv), strings.ReplaceAll(p.info.ObjectOf(f.node.(*ast.FuncDecl).Name).String(), f.pkg.Name+".", ""))
	// fmt.Fprintf(p.w, "%s%s()\n", strings.Repeat(p.indent, p.lv), f.Name)
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
