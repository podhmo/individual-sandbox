package main

import (
	"fmt"
	"reflect"
	"text/template"
)

// type Template struct {
// 	name string
// 	*parse.Tree
// 	*common
// 	leftDelim  string
// 	rightDelim string
// }

// // common holds the information shared by related templates.
// type common struct {
// 	tmpl   map[string]*Template // Map from name to defined templates.
// 	option option
// 	// We use two maps, one for parsing and one for execution.
// 	// This separation makes the API cleaner since it doesn't
// 	// expose reflection to the client.
// 	muFuncs    sync.RWMutex // protects parseFuncs and execFuncs
// 	parseFuncs FuncMap
// 	execFuncs  map[string]reflect.Value
// }

func main() {
	tmpl := template.Must(template.New("root").Parse(`hello {{.Value}}`))
	tmpl.Funcs(map[string]interface{}{
		"increment": func(n int) int { return n + 1 },
	})

	rv := reflect.ValueOf(tmpl).Elem()
	{
		rt := rv.Type()
		// tmpl
		fmt.Println(rt)
		for i := 0; i < rt.NumField(); i++ {
			fmt.Println("	", rt.Field(i).Name, rt.Field(i).Type)
		}
	}
	fmt.Println("")

	common := rv.FieldByName("common")
	{
		rt := common.Type().Elem()
		// tmpl
		fmt.Println(rt)
		for i := 0; i < rt.NumField(); i++ {
			fmt.Println("	", rt.Field(i).Name, rt.Field(i).Type)
		}
	}
}
