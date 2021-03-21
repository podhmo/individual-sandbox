package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"reflect"
	"regexp"
	"strings"

	"github.com/podhmo/reflect-openapi/pkg/arglist"
)

type Symbol struct {
	Name    string
	Package *Package
}

type Package struct {
	Name string
	Path string
}

type EmitFunc func(ctx context.Context, w io.Writer, filename string) error
type Code struct {
	*Symbol
	Emit    EmitFunc
	Deps    []*Code
	Imports []string
}

// ----------------------------------------

type Binder struct {
	Lookup       *Lookup
	Dependencies *Dependencies
	Package      *Package
}

func (b *Binder) MustBindAction(path string, action interface{}) *Code {
	code, err := b.BindAction(path, action)
	if err != nil {
		panic(err)
	}
	return code
}
func (b *Binder) BindAction(path string, action interface{}) (*Code, error) {
	rt := reflect.TypeOf(action)
	if rt.Kind() != reflect.Func {
		return nil, fmt.Errorf("mismatch type must be the value (kind=Func)")
	}

	argset, err := arglist.NewLookup().LookupNameSetFromFunc(action)
	if err != nil {
		return nil, err
	}

	pathvars := b.Lookup.PathVars(path)

	type info struct {
		Name  string
		Type  ParamType
		Value reflect.Type
	}
	var fields []info
	for i := 0; i < rt.NumIn(); i++ {
		name := argset.Args[i]
		arg := rt.In(i)
		pt := b.Lookup.ParamType(name, arg, pathvars)
		b.Dependencies.Add(name, arg, pt)
		switch pt {
		case ParamTypeData, ParamTypePath, ParamTypeQuery:
			fields = append(fields, info{Name: name, Type: pt, Value: arg})
		}
	}

	funcName := fmt.Sprintf("Bind")
	typeName := fmt.Sprintf("%sInput", argset.Name)

	imports := make([]string, 0, len(fields))
	for _, info := range fields {
		imports = append(imports, info.Value.PkgPath())
	}
	imports = append(imports, "strconv")
	imports = append(imports, "github.com/go-chi/chi/v5")
	return &Code{
		Symbol: &Symbol{
			Name:    funcName,
			Package: b.Package,
		},
		Imports: imports,
		Emit: func(ctx context.Context, w io.Writer, filename string) error {
			fmt.Fprintf(w, "// Bind :\n")
			fmt.Fprintf(w, "func (input *%s) Bind(req *http.Request) error {\n", typeName)

			hasData := false
			hasQueryString := false
			for _, info := range fields {
				switch info.Type {
				case ParamTypePath:
					switch rt.Kind() {
					case reflect.Bool:
						fmt.Fprintf(w, "	if v, ok := strconv.ParseBool(chi.URLParam(req, %q)); ok {\n", info.Name)
						fmt.Fprintf(w, "		input.%s%s = &v\n", strings.ToUpper(info.Name[0:1]), info.Name[1:])
					case reflect.Int:
						fmt.Fprintf(w, "	if v, ok := strconv.ParseInt(chi.URLParam(req, %q), 64); ok {\n", info.Name)
						fmt.Fprintf(w, "		v := int(v)\n")
						fmt.Fprintf(w, "		input.%s%s = &v\n", strings.ToUpper(info.Name[0:1]), info.Name[1:])

					case reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
						fmt.Fprintf(w, "	if v, ok := strconv.ParseInt(chi.URLParam(req, %q), 10, 64); ok {\n", info.Name)
						fmt.Fprintf(w, "		v := %s(v)\n", rt.Kind())
						fmt.Fprintf(w, "		input.%s%s = &v\n", strings.ToUpper(info.Name[0:1]), info.Name[1:])
					case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
						fmt.Fprintf(w, "	if v, ok := strconv.ParseUint(chi.URLParam(req, %q), 10, 64); ok {\n", info.Name)
						fmt.Fprintf(w, "		v := %s(v)\n", rt.Kind())
						fmt.Fprintf(w, "		input.%s%s = &v\n", strings.ToUpper(info.Name[0:1]), info.Name[1:])
					case reflect.Float32, reflect.Float64:
						fmt.Fprintf(w, "	if v, ok := strconv.ParseFloat(chi.URLParam(req, %q), 64); ok {\n", info.Name)
						fmt.Fprintf(w, "		v := %s(v)\n", rt.Kind())
						fmt.Fprintf(w, "		input.%s%s = &v\n", strings.ToUpper(info.Name[0:1]), info.Name[1:])
					default:
						fmt.Fprintf(w, "	{\n")
						fmt.Fprintf(w, "		v := chi.URLParam(req, %q)\n", info.Name)
						fmt.Fprintf(w, "		input.%s%s = &v\n", strings.ToUpper(info.Name[0:1]), info.Name[1:])
					}
					fmt.Fprintf(w, "	}\n")
				case ParamTypeQuery:
					if !hasQueryString {
						hasQueryString = true
						fmt.Fprintf(w, "	q := req.URL.Query()\n")
					}
					rt := info.Value.Elem()
					switch rt.Kind() {
					case reflect.Bool:
						fmt.Fprintf(w, "	if v, ok := strconv.ParseBool(q.Get(%q)); ok {\n", info.Name)
						fmt.Fprintf(w, "		input.%s%s = &v\n", strings.ToUpper(info.Name[0:1]), info.Name[1:])
					case reflect.Int:
						fmt.Fprintf(w, "	if v, ok := strconv.ParseInt(q.Get(%q), 64); ok {\n", info.Name)
						fmt.Fprintf(w, "		v := int(v)\n")
						fmt.Fprintf(w, "		input.%s%s = &v\n", strings.ToUpper(info.Name[0:1]), info.Name[1:])

					case reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
						fmt.Fprintf(w, "	if v, ok := strconv.ParseInt(q.Get(%q), 10, 64); ok {\n", info.Name)
						fmt.Fprintf(w, "		v := %s(v)\n", rt.Kind())
						fmt.Fprintf(w, "		input.%s%s = &v\n", strings.ToUpper(info.Name[0:1]), info.Name[1:])
					case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
						fmt.Fprintf(w, "	if v, ok := strconv.ParseUint(q.Get(%q), 10, 64); ok {\n", info.Name)
						fmt.Fprintf(w, "		v := %s(v)\n", rt.Kind())
						fmt.Fprintf(w, "		input.%s%s = &v\n", strings.ToUpper(info.Name[0:1]), info.Name[1:])
					case reflect.Float32, reflect.Float64:
						fmt.Fprintf(w, "	if v, ok := strconv.ParseFloat(q.Get(%q), 64); ok {\n", info.Name)
						fmt.Fprintf(w, "		v := %s(v)\n", rt.Kind())
						fmt.Fprintf(w, "		input.%s%s = &v\n", strings.ToUpper(info.Name[0:1]), info.Name[1:])
					default:
						fmt.Fprintf(w, "	{\n")
						fmt.Fprintf(w, "		v := q.Get(%q)\n", info.Name)
						fmt.Fprintf(w, "		input.%s%s = &v\n", strings.ToUpper(info.Name[0:1]), info.Name[1:])
					}
					fmt.Fprintf(w, "	}\n")
				case ParamTypeData:
					hasData = true
				}
			}

			if hasData {
				fmt.Fprintf(w, "	if err := json.NewDecoder(r.Body).Decode(input); err != nil {\n")
				fmt.Fprintf(w, "		return err\n")
				fmt.Fprintf(w, "	}\n")
				fmt.Fprintf(w, "	defer req.Body.Close()\n")
			}
			fmt.Fprintf(w, "	return nil\n")
			fmt.Fprintf(w, "}\n")
			return nil
		},
		Deps: []*Code{
			{
				Symbol: &Symbol{
					Name:    typeName,
					Package: b.Package,
				},
				Emit: func(ctx context.Context, w io.Writer, filename string) error {
					fmt.Fprintf(w, "// %s :\n", typeName)
					fmt.Fprintf(w, "type %s{\n", typeName)
					for _, info := range fields {
						if info.Type == ParamTypeData {
							fmt.Fprintf(w, "	%s%s %s `json:%q` // as %s\n", strings.ToUpper(info.Name[0:1]), info.Name[1:], info.Value, info.Name, info.Type)
						} else {
							fmt.Fprintf(w, "	%s%s %s // as %s\n", strings.ToUpper(info.Name[0:1]), info.Name[1:], info.Value, info.Type)
						}
					}
					fmt.Fprintf(w, "}\n")
					return nil
				},
			},
		},
	}, nil
}

type Dependencies struct {
	Order []reflect.Type
	Data  map[reflect.Type]bool
}

func (d *Dependencies) Add(name string, rt reflect.Type, typ ParamType) {
	if typ != ParamTypeComponent {
		return
	}
	// TODO: conflict check
	if _, ok := d.Data[rt]; ok {
		return
	}
	d.Data[rt] = true
	d.Order = append(d.Order, rt)
}

type ParamType string

const (
	ParamTypeInvalid   ParamType = "invalid"
	ParamTypeIgnored             = "ignored" // e.g. context.Context
	ParamTypeComponent           = "component"
	ParamTypePath                = "path"
	ParamTypeData                = "data"  // json body
	ParamTypeQuery               = "query" // query string
)

type Lookup struct {
	PathPattern *regexp.Regexp
}

func (l *Lookup) PathVars(path string) []string {
	// TODO: support int
	matched := l.PathPattern.FindAllString(path, -1)
	r := make([]string, len(matched))
	for i := range matched {
		r[i] = strings.TrimSpace(matched[i][1 : len(matched[i])-1])
	}
	return r
}

func (l *Lookup) ParamType(name string, rt reflect.Type, pathVars []string) ParamType {
	switch rt.Kind() {
	case reflect.Func, reflect.Interface:
		return ParamTypeComponent
	case reflect.Ptr:
		switch rt.Elem().Kind() {
		case reflect.Bool, reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
			reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64,
			reflect.Float32, reflect.Float64,
			reflect.String:
			return ParamTypeQuery
		default:
			return ParamTypeComponent
		}
	case reflect.Bool, reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64,
		reflect.Float32, reflect.Float64,
		reflect.String:
		for _, x := range pathVars {
			if name == x {
				return ParamTypePath
			}
		}
		return ParamTypeData
	case reflect.Map, reflect.Slice, reflect.Struct:
		return ParamTypeData
	default:
		return ParamTypeInvalid
	}
}

func NewBinder(pkg *Package) *Binder {
	return &Binder{
		Lookup: &Lookup{
			PathPattern: regexp.MustCompile(`{[^}]+}`),
		},
		Dependencies: &Dependencies{
			Data: map[reflect.Type]bool{},
		},
		Package: pkg,
	}
}

type Foo struct {
	Name string
}
type Greeter interface {
	Greet(target string) string
}

func UpdateFoo(ctx context.Context, foo Foo, xxxId string) error {
	return nil
}
func Hello(ctx context.Context, greeter Greeter, target string, pretty *bool, xxx *string, n *int8, m *float32) error {
	return nil
}

func Emit(w io.Writer, code *Code) error {
	// todo: seen handling

	name := ""
	ctx := context.Background()
	for _, subcode := range code.Deps {
		if err := subcode.Emit(ctx, w, name); err != nil {
			return err
		}
	}
	if err := code.Emit(ctx, w, name); err != nil {
		return err
	}
	fmt.Fprintf(w, "\n")
	return nil
}

func main() {
	b := NewBinder(&Package{Name: "foo"})
	{
		code := b.MustBindAction("/foo/{xxxId}", UpdateFoo)
		Emit(os.Stdout, code)
	}
	{
		code := b.MustBindAction("/hello/{ target }", Hello)
		Emit(os.Stdout, code)
	}
}
