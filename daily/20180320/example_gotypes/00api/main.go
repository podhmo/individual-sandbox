package main

import (
	"fmt"
	"go/types"
	"log"

	"golang.org/x/tools/go/loader"
)

// S1 :
type S1 string

// S2 :
type S2 S1

// S3 :
type S3 = string

func (s S1) String() string {
	return "s1"
}

func (s *S2) String() string {
	return "s2"
}

func main() {
	c := loader.Config{}
	c.Import(".")
	prog, err := c.Load()
	if err != nil {
		log.Fatal(err)
	}

	info := prog.Package(".")
	s := info.Pkg.Scope()
	p := func(t types.Type) string {
		return types.TypeString(t, types.RelativeTo(info.Pkg))
	}

	{
		candidates := []string{"S1", "S2", "S3"}

		fmt.Println("## type and underlying")
		fmt.Println("")
		fmt.Println("|name|type|underlying|")
		fmt.Println("|:--|:--|:--|")
		for _, name := range candidates {
			fmt.Printf("|%s|%s|%s|\n", name, p(s.Lookup(name).Type()), p(s.Lookup(name).Type().Underlying()))
		}
	}
	fmt.Println("")
	fmt.Println("## go/types's api (for type)")
	fmt.Println("")
	{
		candidates := []types.Type{
			s.Lookup("S1").Type(),
			s.Lookup("S2").Type(),
			s.Lookup("S3").Type(),
			types.NewPointer(s.Lookup("S1").Type()),
			types.NewPointer(s.Lookup("S2").Type()),
			types.NewPointer(s.Lookup("S3").Type()),
			types.Universe.Lookup("string").Type(),
			types.Universe.Lookup("int").Type(),
			types.NewPointer(types.Universe.Lookup("string").Type()),
		}

		fmt.Println("|T0|T1|ConvertibleTo(T0,T1)|ConvertibleTo(T1,T0)|AssignableTo(T0,T1)|AssignableTo(T1,T0)|Identical(T0,t1)")
		fmt.Println("|:--|:--|:--|:--|:--|:--|:--|")
		for i := range candidates {
			for j := i + 1; j < len(candidates); j++ {
				t0 := candidates[i]
				t1 := candidates[j]
				fmt.Printf(
					"|%s|%s|%v|%v|%v|%v|%v|\n",
					p(t0),
					p(t1),
					types.ConvertibleTo(t0, t1),
					types.ConvertibleTo(t1, t0),
					types.AssignableTo(t0, t1),
					types.AssignableTo(t1, t0),
					types.Identical(t0, t1),
				)
			}
		}
	}
	fmt.Println("")
	fmt.Println("## go/types's api (for interface)")
	fmt.Println("")
	{
		t1 := prog.Package("fmt").Pkg.Scope().Lookup("Stringer").Type()
		iface := t1.Underlying().(*types.Interface)

		candidates := []types.Type{
			s.Lookup("S1").Type(),
			s.Lookup("S2").Type(),
			s.Lookup("S3").Type(),
			types.NewPointer(s.Lookup("S1").Type()),
			types.NewPointer(s.Lookup("S2").Type()),
			types.NewPointer(s.Lookup("S3").Type()),
			types.Universe.Lookup("string").Type(),
			types.Universe.Lookup("int").Type(),
			types.NewPointer(types.Universe.Lookup("string").Type()),
		}
		fmt.Println("|iface|T0|AssertableTo(iface,T0)|Implements(T0,iface)|")
		fmt.Println("|:--|:--|:--|:--|")
		for i := range candidates {
			t0 := candidates[i]
			fmt.Printf(
				"|%s|%s|%v|%v|\n",
				p(t1),
				p(t0),
				types.AssertableTo(iface, t0),
				types.Implements(t0, iface),
			)
		}
	}
}
