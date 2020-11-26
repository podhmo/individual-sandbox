package main

import (
	"fmt"
	"m/extract"
	"strings"
	"text/template/parse"
)

func main() {
	source := `
// {{.Name}} :
type {{.Name}} struct {
{{ range $i, $field := .Fields }}
	{{ $field.Name | Upper }} string
{{ end }}
}
`
	// Env $field <- .Fields[$i]
	// $field.Name is string
	// Field is struct { Name string }  as $field
	// .Fields is []$field or map[Any]$field
	// .Name is Any
	// . is struct { Name Any; Fields []$field}

	// 全部を解釈するのは、無理。この場合は何が取れれば良いんだろう？
	// .Name と .Fields かな。
	// ToplevelのListNode内のActionNodeとRangeNode,WithNode,IfNodeのPipeを見れば良い？

	fMap := map[string]interface{}{
		"Upper": strings.ToUpper,
	}
	fmt.Println(extract.Extract(source, fMap))

	m, _ := parse.Parse("x", source, "", "", fMap)
	fmt.Println(m["x"].Root.String()) // コレで復旧しているのか
}

type Params struct {
	Name   string
	Fields []Field
}
type Field struct {
	Name string
}
