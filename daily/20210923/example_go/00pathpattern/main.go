package main

import (
	"encoding/json"
	"os"
	"strings"

	"github.com/podhmo/apikit/web/pathpattern"
)

func main() {
	pathpattern.DefaultOptions.SupportRegExp = true

	root := &pathpattern.Node{}
	root.MustAdd("GET /article/{articleId}", "get article", nil)
	root.MustAdd("GET /article/{articleId}/comment/{commentId}", "get comment", nil)
	root.MustAdd("GET /article/{articleId}/comment/", "list comment", nil)
	root.MustAdd("GET /article/{articleId}/comment", "list comment2", nil)
	// pp.Println(root)

	enc := json.NewEncoder(os.Stdout)
	pathpattern.Walk(root, func(n *pathpattern.WalkerNode) error {
		return enc.Encode(map[string]interface{}{
			"path":      strings.Join(n.Path(), ""),
			"variables": n.Node.VariableNames,
			"value":     n.Node.Value,
		})
	})
}
