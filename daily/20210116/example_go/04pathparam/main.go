package main

import (
	"fmt"
	"m/pathparam"
	"strings"
)

type P struct {
	ID    string `json:"id" index:"literal"`
	Grade string `json:"grade" index:"enum" enum:"A,B,C,D,E"`
	Type  string `json:"type" index:"dynamic-enum"`
}

func main() {
	{
		fmt.Println("----------------------------------------")
		p := P{ID: "XXX"}
		pathparam.Walk(p, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		})
	}
	{
		fmt.Println("----------------------------------------")
		p := P{ID: "XXX", Grade: "C"}
		pathparam.Walk(p, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		})
	}
	{
		fmt.Println("----------------------------------------")
		p := P{ID: "XXX", Grade: "C"}
		pathparam.Walk(p, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		},
			pathparam.WithBreakdowns("grade"),
		)
	}
	{
		fmt.Println("----------------------------------------")
		p := P{ID: "XXX", Grade: "C"}
		pathparam.Walk(p, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		},
			pathparam.WithBreakdowns("type"),
			pathparam.WithDynamicEnum("type", func(p interface{}) []string {
				return []string{"x", "y", "z"}
			}),
		)
	}
	{
		fmt.Println("----------------------------------------")
		p := P{ID: "XXX", Grade: "C"}
		pathparam.Walk(p, func(parts []string) {
			fmt.Printf("%#+v	%q\n", p, strings.Join(parts, "/"))
		},
			pathparam.WithBreakdowns("type", "grade"),
			pathparam.WithDynamicEnum("type", func(p interface{}) []string {
				return []string{"x", "y", "z"}
			}),
		)
	}
}
