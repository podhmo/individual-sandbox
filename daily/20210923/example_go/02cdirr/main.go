package main

import (
	"fmt"
	"strings"

	"github.com/gookit/color"
	"github.com/shibukawa/cdiff"
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
	diff := cdiff.Diff(string(oldDoc), string(newDoc), cdiff.WordByWord)
	fmt.Println(diff)
	color.Print(diff.UnifiedWithGooKitColor("got", "want", 3, cdiff.GooKitColorTheme))
}
