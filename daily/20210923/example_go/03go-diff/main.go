package main

import (
	"strings"

	"github.com/gookit/color"
	"github.com/sergi/go-diff/diffmatchpatch"
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

	dmp := diffmatchpatch.New()
	diffs := dmp.DiffMain(oldDoc, newDoc, false)
	color.Print(dmp.DiffPrettyText(diffs))
	color.Print(dmp.DiffText1(diffs))
	color.Print(dmp.DiffText2(diffs))
}
