package main

import (
	"fmt"
	"strings"

	"github.com/gookit/color"
	"github.com/hexops/gotextdiff"
	"github.com/hexops/gotextdiff/myers"
	"github.com/hexops/gotextdiff/span"
)

func main() {
	oldDoc := strings.TrimSpace(`
GET /articles/{articleId}
GET /articles/{articleId}/comments
GET /articles/{articleId}/comments/{commentId}
`)
	newDoc := strings.TrimSpace(`
GET /articles/{articleId}
GET /articles/{articleId}/{articleId}
GET /articles/{articleId}/{articleId}/{commentId}
`)

	edits := myers.ComputeEdits(span.URIFromPath("a.txt"), oldDoc, newDoc)
	diff := fmt.Sprint(gotextdiff.ToUnified("a.txt", "b.txt", oldDoc, edits))
	color.Println(diff)
}
