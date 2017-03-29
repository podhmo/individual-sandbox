package main

import (
	"fmt"
	"net/url"
)

// url.QueryEscape is python's quote_plus
func quote(s string) string {
	return (&url.URL{Path: s}).RequestURI()
}

func main() {
	fmt.Println("…", "->", quote("…"))
	fmt.Println("-", "->", quote("-"))
	fmt.Println("_", "->", quote("_"))
	fmt.Println(quote("_") < quote("-"))
	fmt.Println("/", "->", quote("/"))
	fmt.Println("wacul-ai.com/blog/ga-keyword/", "->", quote("wacul-ai.com/blog/ga-keyword/"))
	fmt.Println("wacul-ai.com/blog/ga-keywordユニバーサルカイロ", "->", quote("wacul-ai.com/blog/ga-keywordユニバーサルカイロ"))
	fmt.Println(quote("wacul-ai.com/blog/ga-keyword/") > quote("wacul-ai.com/blog/ga-keywordユニバーサルカイロ"))
}
