package main

import (
	"encoding/json"
	"log"
	"os"
	"strings"

	"github.com/podhmo/apikit/web"
)

func main() {
	r := web.NewRouter()
	r.Method("GET", "/articles/{articleId}", "get article")
	r.Group("/articles/{articleId}", func(r *web.Router) {
		r.Method("GET", "/comments", "list comments")
		r.Method("GET", "/comments/{commentId}/", "get comment")
	})

	// debug
	enc := json.NewEncoder(os.Stdout)
	if err := web.Walk(r, func(n *web.WalkerNode) error {
		return enc.Encode(map[string]interface{}{
			"path":      strings.Join(n.Path(), ""),
			"variables": n.Node.VariableNames,
			"value":     n.Node.Value,
		})
	}); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
