package main

import (
	"encoding/json"
	"fmt"
	"go/ast"
	"go/token"
	"log"
	"os"

	"golang.org/x/tools/go/packages"
)

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

	// TODO: lookup all methods
	// TODO: traverse body
	// TODO: stack

	p := &Package{
		fset:  fset,
		Funcs: map[string]*Func{},
	}
	for _, f := range pkg.Syntax {
		for _, decl := range f.Decls {
			if decl, ok := decl.(*ast.FuncDecl); ok {
				// TODO: skip unexported
				// TODO: method
				// TODO: dedup
				fun := p.GetOrCreateFuncByDecl(f, decl)
				ast.Inspect(decl.Body, func(node ast.Node) bool {
					if node, ok := node.(*ast.CallExpr); ok {
						switch ob := node.Fun.(type) {
						case *ast.Ident:
							if fun.Recv != "" { // method
								return false
							}
							fun.Calls = append(fun.Calls, p.GetOrCreateFuncByName(ob.Name))
						case *ast.SelectorExpr:
							if fun.Recv == "" { // function
								return false
							}
							// TODO: see type
							if fun.Recv != ob.X.(*ast.Ident).Name { // xxx
								return false
							}
							fun.Calls = append(fun.Calls, p.GetOrCreateFuncByName(ob.Sel.Name))
						default:
							panic(fmt.Sprintf("unexported type: %T", fun))
						}
					}
					return true
				})
			}
		}
	}
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(p); err != nil {
		return err
	}

	for _, f := range p.Funcs {
		fmt.Println(f)
	}
	return nil
}

type Package struct {
	Funcs map[string]*Func `json:"funcs"`
	// TODO: file

	fset *token.FileSet
}

func (p *Package) GetOrCreateFuncByName(name string) *Func {
	f, ok := p.Funcs[name]
	if ok {
		return f
	}
	f = &Func{Name: name, Calls: []*Func{}, Kind: FuncKindFunction}
	p.Funcs[name] = f
	return f
}
func (p *Package) GetOrCreateFuncByDecl(file *ast.File, decl *ast.FuncDecl) *Func {
	name := decl.Name.Name
	f := p.GetOrCreateFuncByName(name)
	f.node = decl
	tf := p.fset.File(file.Pos())
	f.File = tf.Name()
	f.Line = tf.Line(decl.Pos())
	if decl.Recv != nil {
		f.Kind = FuncKindMethod
		f.Recv = decl.Recv.List[0].Names[0].Name // xxx
	}
	return f
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

//   0  *ast.FuncDecl {
//   1  .  Name: *ast.Ident {
//   2  .  .  NamePos: m/internal/s/s.go:16:6
//   3  .  .  Name: "F"
//   4  .  .  Obj: *ast.Object {
//   5  .  .  .  Kind: func
//   6  .  .  .  Name: "F"
//   7  .  .  .  Decl: *(obj @ 0)
//   8  .  .  }
//   9  .  }
//  10  .  Type: *ast.FuncType {
//  11  .  .  Func: m/internal/s/s.go:16:1
//  12  .  .  Params: *ast.FieldList {
//  13  .  .  .  Opening: m/internal/s/s.go:16:7
//  14  .  .  .  Closing: m/internal/s/s.go:16:8
//  15  .  .  }
//  16  .  }
//  17  .  Body: *ast.BlockStmt {
//  18  .  .  Lbrace: m/internal/s/s.go:16:10
//  19  .  .  List: []ast.Stmt (len = 2) {
//  20  .  .  .  0: *ast.ExprStmt {
//  21  .  .  .  .  X: *ast.CallExpr {
//  22  .  .  .  .  .  Fun: *ast.SelectorExpr {
//  23  .  .  .  .  .  .  X: *ast.Ident {
//  24  .  .  .  .  .  .  .  NamePos: m/internal/s/s.go:17:2
//  25  .  .  .  .  .  .  .  Name: "fmt"
//  26  .  .  .  .  .  .  }
//  27  .  .  .  .  .  .  Sel: *ast.Ident {
//  28  .  .  .  .  .  .  .  NamePos: m/internal/s/s.go:17:6
//  29  .  .  .  .  .  .  .  Name: "Println"
//  30  .  .  .  .  .  .  }
//  31  .  .  .  .  .  }
//  32  .  .  .  .  .  Lparen: m/internal/s/s.go:17:13
//  33  .  .  .  .  .  Args: []ast.Expr (len = 1) {
//  34  .  .  .  .  .  .  0: *ast.BasicLit {
//  35  .  .  .  .  .  .  .  ValuePos: m/internal/s/s.go:17:14
//  36  .  .  .  .  .  .  .  Kind: STRING
//  37  .  .  .  .  .  .  .  Value: "\"hello\""
//  38  .  .  .  .  .  .  }
//  39  .  .  .  .  .  }
//  40  .  .  .  .  .  Ellipsis: -
//  41  .  .  .  .  .  Rparen: m/internal/s/s.go:17:21
//  42  .  .  .  .  }
//  43  .  .  .  }
//  44  .  .  .  1: *ast.ExprStmt {
//  45  .  .  .  .  X: *ast.CallExpr {
//  46  .  .  .  .  .  Fun: *ast.Ident {
//  47  .  .  .  .  .  .  NamePos: m/internal/s/s.go:18:2
//  48  .  .  .  .  .  .  Name: "f"
//  49  .  .  .  .  .  .  Obj: *ast.Object {
//  50  .  .  .  .  .  .  .  Kind: func
//  51  .  .  .  .  .  .  .  Name: "f"
//  52  .  .  .  .  .  .  .  Decl: *ast.FuncDecl {
//  53  .  .  .  .  .  .  .  .  Name: *ast.Ident {
//  54  .  .  .  .  .  .  .  .  .  NamePos: m/internal/s/s.go:20:6
//  55  .  .  .  .  .  .  .  .  .  Name: "f"
//  56  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 49)
//  57  .  .  .  .  .  .  .  .  }
//  58  .  .  .  .  .  .  .  .  Type: *ast.FuncType {
//  59  .  .  .  .  .  .  .  .  .  Func: m/internal/s/s.go:20:1
//  60  .  .  .  .  .  .  .  .  .  Params: *ast.FieldList {
//  61  .  .  .  .  .  .  .  .  .  .  Opening: m/internal/s/s.go:20:7
//  62  .  .  .  .  .  .  .  .  .  .  Closing: m/internal/s/s.go:20:8
//  63  .  .  .  .  .  .  .  .  .  }
//  64  .  .  .  .  .  .  .  .  }
//  65  .  .  .  .  .  .  .  .  Body: *ast.BlockStmt {
//  66  .  .  .  .  .  .  .  .  .  Lbrace: m/internal/s/s.go:20:10
//  67  .  .  .  .  .  .  .  .  .  List: []ast.Stmt (len = 1) {
//  68  .  .  .  .  .  .  .  .  .  .  0: *ast.ExprStmt {
//  69  .  .  .  .  .  .  .  .  .  .  .  X: *ast.CallExpr {
//  70  .  .  .  .  .  .  .  .  .  .  .  .  Fun: *ast.SelectorExpr {
//  71  .  .  .  .  .  .  .  .  .  .  .  .  .  X: *ast.Ident {
//  72  .  .  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: m/internal/s/s.go:21:2
//  73  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Name: "fmt"
//  74  .  .  .  .  .  .  .  .  .  .  .  .  .  }
//  75  .  .  .  .  .  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
//  76  .  .  .  .  .  .  .  .  .  .  .  .  .  .  NamePos: m/internal/s/s.go:21:6
//  77  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Name: "Println"
//  78  .  .  .  .  .  .  .  .  .  .  .  .  .  }
//  79  .  .  .  .  .  .  .  .  .  .  .  .  }
//  80  .  .  .  .  .  .  .  .  .  .  .  .  Lparen: m/internal/s/s.go:21:13
//  81  .  .  .  .  .  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
//  82  .  .  .  .  .  .  .  .  .  .  .  .  .  0: *ast.BasicLit {
//  83  .  .  .  .  .  .  .  .  .  .  .  .  .  .  ValuePos: m/internal/s/s.go:21:14
//  84  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Kind: STRING
//  85  .  .  .  .  .  .  .  .  .  .  .  .  .  .  Value: "\"hello\""
//  86  .  .  .  .  .  .  .  .  .  .  .  .  .  }
//  87  .  .  .  .  .  .  .  .  .  .  .  .  }
//  88  .  .  .  .  .  .  .  .  .  .  .  .  Ellipsis: -
//  89  .  .  .  .  .  .  .  .  .  .  .  .  Rparen: m/internal/s/s.go:21:21
//  90  .  .  .  .  .  .  .  .  .  .  .  }
//  91  .  .  .  .  .  .  .  .  .  .  }
//  92  .  .  .  .  .  .  .  .  .  }
//  93  .  .  .  .  .  .  .  .  .  Rbrace: m/internal/s/s.go:22:1
//  94  .  .  .  .  .  .  .  .  }
//  95  .  .  .  .  .  .  .  }
//  96  .  .  .  .  .  .  }
//  97  .  .  .  .  .  }
//  98  .  .  .  .  .  Lparen: m/internal/s/s.go:18:3
//  99  .  .  .  .  .  Ellipsis: -
// 100  .  .  .  .  .  Rparen: m/internal/s/s.go:18:4
// 101  .  .  .  .  }
// 102  .  .  .  }
// 103  .  .  }
// 104  .  .  Rbrace: m/internal/s/s.go:19:1
// 105  .  }
// 106  }
