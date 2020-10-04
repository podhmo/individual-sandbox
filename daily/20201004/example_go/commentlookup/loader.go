package commentlookup

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/ioutil"
	"reflect"
	"runtime"
	"strings"
)

type Region struct {
	File   *ast.File
	Name   string
	Lineno int
	cache  map[string]*ast.FuncDecl
}

func (r *Region) LookupComment(name string) *ast.CommentGroup {
	if len(r.cache) == 0 {
		r.cache[""] = nil // mark
		for _, decl := range r.File.Decls {
			if decl, ok := decl.(*ast.FuncDecl); ok {
				r.cache[decl.Name.Name] = decl
			}
		}
	}
	decl, ok := r.cache[name]
	if !ok || decl == nil {
		return nil
	}
	return decl.Doc
}

func (r *Region) Comment() *ast.CommentGroup {
	// TODO: validate by lineno?
	// TODO: support methods?
	return r.LookupComment(strings.TrimPrefix(r.Name, r.File.Name.Name+"."))
}

func (r *Region) CommentText() string {
	return ExtractText(r.Comment())
}

type Lookup struct {
	fset *token.FileSet

	fileCache map[string]*ast.File
	declCache map[*ast.File]map[string]*ast.FuncDecl
}

func NewLookup() *Lookup {
	return &Lookup{
		fset:      token.NewFileSet(),
		fileCache: map[string]*ast.File{},
		declCache: map[*ast.File]map[string]*ast.FuncDecl{},
	}
}

func (l *Lookup) LookupAST(filename string) (*ast.File, error) {
	if f, ok := l.fileCache[filename]; ok {
		return f, nil
	}
	mode := parser.ParseComments
	code, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	f, err := parser.ParseFile(l.fset, filename, code, mode)
	if err != nil {
		return nil, err
	}
	l.fileCache[filename] = f
	return f, nil
}

func (l *Lookup) LookupRegion(filename string, lineno int, targerName string) (Region, error) {
	f, err := l.LookupAST(filename)
	if err != nil {
		return Region{}, nil
	}
	cache, ok := l.declCache[f]
	if !ok {
		cache = map[string]*ast.FuncDecl{}
		l.declCache[f] = cache
	}
	return Region{File: f, Name: targerName, Lineno: lineno, cache: cache}, nil
}

func (l *Lookup) LookupRegionFromFunc(fn interface{}) (Region, error) {
	rfunc := runtime.FuncForPC(reflect.ValueOf(fn).Pointer())
	filename, lineno := rfunc.FileLine(rfunc.Entry())
	name := rfunc.Name()
	return l.LookupRegion(filename, lineno, name)
}

func (l *Lookup) LookupCommentTextFromFunc(fn interface{}) (string, error) {
	region, err := l.LookupRegionFromFunc(fn)
	if err != nil {
		return "", err
	}
	return region.CommentText(), nil
}

func ExtractText(cg *ast.CommentGroup) string {
	if cg == nil {
		return ""
	}
	buf := make([]string, len(cg.List))
	for i, c := range cg.List {
		buf[i] = strings.TrimPrefix(c.Text, "// ")
	}
	return strings.Join(buf, "\n")
}
