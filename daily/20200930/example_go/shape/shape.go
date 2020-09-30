package shape

import (
	"fmt"
	"reflect"
	"runtime"
	"strconv"
	"strings"
)

// TODO: 埋め込み
// TODO: コメント
// TODO: tag

type Kind reflect.Kind

func (k Kind) MarshalJSON() ([]byte, error) {
	return []byte(fmt.Sprintf(`%q`, k.String())), nil
}
func (k Kind) String() string {
	return reflect.Kind(k).String()
}

type Shape interface {
	Shape() string

	GetName() string
	GetPackage() string
	GetKind() reflect.Kind

	inc()
}
type ShapeList []Shape

type ShapeMap struct {
	Keys   []string `json:"keys"`
	Values []Shape  `json:"values"`
}

type Info struct {
	Kind    Kind        `json:"kind"`
	Name    string      `json:"name"`
	Lv      int         `json:"lv"` // v is 0, *v is 1
	Package string      `json:"package"`
	raw     interface{} `json:"-"`
}

func (v *Info) inc() {
	v.Lv++
}

func (v *Info) Shape() string {
	return v.Kind.String()
}
func (v *Info) GetName() string {
	return v.Name
}
func (v *Info) GetFullName() string {
	return strings.TrimPrefix(v.Package+"."+v.Name, ".")
}
func (v *Info) GetPackage() string {
	return v.Package
}
func (v *Info) GetKind() reflect.Kind {
	return reflect.Kind(v.Kind)
}

type Primitive struct {
	*Info
}

func (v Primitive) Format(f fmt.State, c rune) {
	fmt.Fprintf(f, "%s%s",
		strings.Repeat("*", v.Lv),
		v.GetFullName(),
	)
}

type Struct struct {
	*Info
	Fields ShapeMap `json:"fields"`
	Tags   []reflect.StructTag
}

func (v Struct) Format(f fmt.State, c rune) {
	if c == 'v' && f.Flag('+') {
		fmt.Fprintf(f, "%s%s{%s}",
			strings.Repeat("*", v.Lv),
			v.GetFullName(),
			strings.Join(v.Fields.Keys, ", "),
		)
		return
	}
	fmt.Fprintf(f, "%s%s",
		strings.Repeat("*", v.Lv),
		v.GetFullName(),
	)
}

type Interface struct {
	*Info
	Methods ShapeMap `json:"methods"`
}

func (v Interface) Format(f fmt.State, c rune) {
	if c == 'v' && f.Flag('+') {
		fmt.Fprintf(f, "%s%s{%s}",
			strings.Repeat("*", v.Lv),
			v.GetFullName(),
			strings.Join(v.Methods.Keys, "(), "),
		)
		return
	}
	fmt.Fprintf(f, "%s%s",
		strings.Repeat("*", v.Lv),
		v.GetFullName(),
	)
}

type Container struct {
	*Info
	Args ShapeList `json:"args"`
}

func (v Container) Format(f fmt.State, c rune) {
	expr := "%v"
	if c == 'v' && f.Flag('+') {
		expr = "%+v"
	}
	args := make([]string, len(v.Args))
	for i := range v.Args {
		args[i] = fmt.Sprintf(expr, v.Args[i])
	}

	fmt.Fprintf(f, "%s%s[%s]",
		strings.Repeat("*", v.Lv),
		v.GetFullName(),
		strings.Join(args, ", "),
	)
}

type Function struct {
	*Info
	Params  ShapeMap `json:"params"`  // for function's In
	Returns ShapeMap `json:"returns"` // for function's Out
}

func (v Function) Format(f fmt.State, c rune) {
	expr := "%v"
	if c == 'v' && f.Flag('+') {
		expr = "%+v"
	}

	params := make([]string, len(v.Params.Keys))
	for i, val := range v.Params.Values {
		params[i] = fmt.Sprintf(expr, val)
	}
	returns := make([]string, len(v.Returns.Keys))
	for i, val := range v.Returns.Values {
		returns[i] = fmt.Sprintf(expr, val)
	}
	fmt.Fprintf(f, "%s%s(%s) (%s)",
		strings.Repeat("*", v.Lv),
		v.GetFullName(),
		strings.Join(params, ", "),
		strings.Join(returns, ", "),
	)
}

func Extract(ob interface{}) Shape {
	rt := reflect.TypeOf(ob)
	path := []string{""}
	history := []reflect.Type{rt}
	return extract(path, history, ob)
}

func extract(path []string, history []reflect.Type, ob interface{}) Shape {
	// if len(path) > 10 {
	// 	panic("x")
	// }
	rt := history[len(history)-1]
	name := rt.Name()
	kind := rt.Kind()
	pkgPath := rt.PkgPath()

	// todo: switch
	switch kind {
	case reflect.Ptr:
		s := extract(
			append(path, "*"),
			append(history, rt.Elem()),
			nil)
		s.inc()
		return s
	case reflect.Slice, reflect.Array, reflect.Chan:
		args := []Shape{
			extract(
				append(path, "slice[0]"),
				append(history, rt.Elem()),
				nil,
			),
		}
		s := Container{
			Args: args,
			Info: &Info{
				Name:    kind.String(), // slice
				Kind:    Kind(kind),
				Package: pkgPath,
				raw:     ob,
			},
		}
		return s
	case reflect.Map:
		args := []Shape{
			extract(
				append(path, "map[0]"),
				append(history, rt.Key()),
				nil,
			),
			extract(
				append(path, "map[1]"),
				append(history, rt.Elem()),
				nil,
			),
		}
		s := Container{
			Args: args,
			Info: &Info{
				Name:    kind.String(), // slice
				Kind:    Kind(kind),
				Package: pkgPath,
				raw:     ob,
			},
		}
		return s
	case reflect.Struct:
		names := make([]string, rt.NumField())
		fields := make([]Shape, rt.NumField())
		tags := make([]reflect.StructTag, rt.NumField())
		for i := 0; i < len(fields); i++ {
			f := rt.Field(i)
			names[i] = f.Name
			fields[i] = extract(
				append(path, "struct."+f.Name),
				append(history, f.Type),
				nil,
			)
			tags[i] = f.Tag
			// todo: anonymous
		}
		s := Struct{
			Fields: ShapeMap{
				Keys:   names,
				Values: fields,
			},
			Tags: tags,
			Info: &Info{
				Name:    name,
				Kind:    Kind(kind),
				Package: pkgPath,
				raw:     ob,
			},
		}
		return s
	case reflect.Func:
		if ob != nil {
			fullname := runtime.FuncForPC(reflect.ValueOf(ob).Pointer()).Name()
			parts := strings.Split(fullname, ".")
			pkgPath = strings.Join(parts[:len(parts)-1], ".")
			name = parts[len(parts)-1]
		}

		pnames := make([]string, rt.NumIn())
		params := make([]Shape, rt.NumIn())
		for i := 0; i < len(params); i++ {
			v := rt.In(i)
			pnames[i] = "args" + strconv.Itoa(i) //
			params[i] = extract(
				append(path, "func.p["+strconv.Itoa(i)+"]"),
				append(history, v),
				nil)
		}

		rnames := make([]string, rt.NumOut())
		returns := make([]Shape, rt.NumOut())
		for i := 0; i < len(returns); i++ {
			v := rt.Out(i)
			rnames[i] = "ret" + strconv.Itoa(i) //
			returns[i] = extract(
				append(path, "func.r["+strconv.Itoa(i)+"]"),
				append(history, v),
				nil)
		}

		s := Function{
			Params:  ShapeMap{Keys: pnames, Values: params},
			Returns: ShapeMap{Keys: rnames, Values: returns},
			Info: &Info{
				Name:    name,
				Kind:    Kind(kind),
				Package: pkgPath,
				raw:     ob,
			},
		}
		return s
	case reflect.Interface:
		names := make([]string, rt.NumMethod())
		methods := make([]Shape, rt.NumMethod())
		for i := 0; i < len(methods); i++ {
			f := rt.Method(i)
			names[i] = f.Name
			methods[i] = extract(
				append(path, "interface."+f.Name),
				append(history, f.Type),
				nil,
			)
		}
		s := Interface{
			Methods: ShapeMap{
				Keys:   names,
				Values: methods,
			},
			Info: &Info{
				Name:    name,
				Kind:    Kind(kind),
				Package: pkgPath,
				raw:     ob,
			},
		}
		return s
	default:
		// fmt.Fprintln(os.Stderr, "\t\t", kind.String())
		s := Primitive{
			Info: &Info{
				Name:    name,
				Kind:    Kind(kind),
				Package: pkgPath,
				raw:     ob,
			},
		}
		return s
	}
}
