package main

import (
	"fmt"
	"log"
	"net/http"

	"github.com/gomarkdown/markdown"
	"github.com/gomarkdown/markdown/html"
	"github.com/gomarkdown/markdown/parser"
)

// https://pkg.go.dev/github.com/gomarkdown/markdown
var p *parser.Parser
var renderer *html.Renderer

func init() {
	extensions := parser.CommonExtensions | parser.AutoHeadingIDs
	p = parser.NewWithExtensions(extensions)

	htmlFlags := html.CommonFlags | html.HrefTargetBlank
	opts := html.RendererOptions{Flags: htmlFlags}
	renderer = html.NewRenderer(opts)
}

func Handler(w http.ResponseWriter, r *http.Request) {
	html := `<!DOCTYPE html>
    <html lang="ja">
    <meta charset="UTF-8">
    <title>%s</title>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown.min.css">
    <style>
        .markdown-body {
            box-sizing: border-box;
            min-width: 200px;
            max-width: 980px;
            margin: 0 auto;
            padding: 45px;
        }
        @media (max-width: 767px) {
            .markdown-body {
                padding: 15px;
            }
        }
    </style>
    <body>
    <article class="markdown-body">
    %s
    </article>
    </body>
    <html>`

	text := []byte(`# hello
	
Hello world.
	
- foo
- bar
- boo

		pre
`)
	fmt.Fprintf(w, html, "hello", markdown.ToHTML(text, p, renderer))

}

func main() {
	log.Println("listen", 8080)
	if err := http.ListenAndServe("localhost:8080", http.HandlerFunc(Handler)); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
