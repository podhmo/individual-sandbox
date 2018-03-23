package main

import (
	"fmt"
	"go/types"
	"log"
	"reflect"
	"sort"

	"golang.org/x/tools/go/loader"
)

// I :
type I struct {
	ob    types.Object
	iface *types.Interface
}

// T :
type T struct {
	ob         types.Object
	underlying *types.Struct
}

func main() {
	c := loader.Config{
		TypeCheckFuncBodies: func(path string) bool {
			return false
		},
	}
	c.Import("io")
	c.Import("bytes")
	prog, err := c.Load()
	if err != nil {
		log.Fatal(err)
	}

	var is []I
	{
		used := map[*types.Interface]struct{}{}
		s := prog.Package("io").Pkg.Scope()
		for _, name := range reflect.ValueOf(s).Elem().FieldByName("elems").MapKeys() {
			ob := s.Lookup(name.String())
			if iface, ok := ob.Type().Underlying().(*types.Interface); ok {
				if _, exists := used[iface]; !exists {
					used[iface] = struct{}{}
					is = append(is, I{ob: ob, iface: iface})
				}
			}
		}
		sort.Slice(is, func(i, j int) bool { return is[i].ob.Name() < is[j].ob.Name() })
	}

	var ts []T
	{
		used := map[*types.Struct]struct{}{}
		s := prog.Package("bytes").Pkg.Scope()
		for _, name := range reflect.ValueOf(s).Elem().FieldByName("elems").MapKeys() {
			ob := s.Lookup(name.String())
			if t, ok := ob.Type().Underlying().(*types.Struct); ok {
				if _, exists := used[t]; !exists {
					used[t] = struct{}{}
					ts = append(ts, T{ob: ob, underlying: t})
				}
			}
		}
		sort.Slice(ts, func(i, j int) bool { return ts[i].ob.Name() < ts[j].ob.Name() })
	}
	fmt.Println("## interface x struct")
	fmt.Println("")
	fmt.Println("|iface|T|AssertableTo(iface,T)|AssertableTo(iface,*T)|Implements(T,iface)|Implements(*T,iface)|")
	fmt.Println("|:--|:--|:--|:--|:--|:--|")
	for _, i := range is {
		for _, t := range ts {
			p := types.NewPointer(t.ob.Type())
			fmt.Printf(
				"|%s|%s|%v|%v|%v|%v|\n",
				i.ob.Type(),
				t.ob.Type(),
				types.AssertableTo(i.iface, t.ob.Type()),
				types.AssertableTo(i.iface, p),
				types.Implements(t.ob.Type(), i.iface),
				types.Implements(p, i.iface),
			)
		}
	}

	fmt.Println("")
	fmt.Println("## interface x interface")
	fmt.Println("")
	fmt.Println("|iface|T|AssertableTo(iface,T)|AssertableTo(iface,*T)|Implements(T,iface)|Implements(*T,iface)|")
	fmt.Println("|:--|:--|:--|:--|:--|:--|")
	for i := range is {
		for j := i + 1; j < len(is); j++ {
			i0 := is[i]
			i1 := is[j]
			p := types.NewPointer(i1.ob.Type())
			fmt.Printf(
				"|%s|%s|%v|%v|%v|%v|\n",
				i0.ob.Type(),
				i1.ob.Type(),
				types.AssertableTo(i0.iface, i1.ob.Type()),
				types.AssertableTo(i0.iface, p),
				types.Implements(i0.ob.Type(), i1.iface),
				types.Implements(p, i0.iface),
			)
		}
	}
}
