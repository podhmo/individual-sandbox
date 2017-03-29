package main

import (
	"fmt"
	"net/url"
	"strings"

	"golang.org/x/exp/utf8string"
)

func urlencode(s string) string {
	// url.QueryEscape is python's quote_plus(). but we need python's quote(). "/" is not "%2f"
	return (&url.URL{Path: s}).RequestURI()
}

func normalize(s string) string {
	// tentative
	// 全角文字列(e.g. "…")は半角文字列より優先される => urlencoded
	// 何故か"_"は"-"(2D0F)より優先される

	xs := strings.Split(s, "/")
	ys := make([]string, len(xs))
	for i, x := range xs {
		if len(x) == 0 {
			ys[i] = x
			continue
		}
		ns := utf8string.NewString(string(x[0]))
		if ns.IsASCII() {
			ys[i] = x
			continue
		}
		ys[i] = urlencode(x)
	}
	return strings.ToLower(strings.Replace(strings.Join(ys, "/"), "_", ",", -1))
}

func main() {
	{
		s := "hai.co.jp/…ge,sp/slidervideo,video,gmocloud,thm.jpg"
		x := utf8string.NewString(s)
		fmt.Println(x.IsASCII())
		fmt.Println(normalize(s))
	}
	{
		s := "hai.co.jp/blog/ga-keywordユニバーサルカイロ"
		x := utf8string.NewString(s)
		fmt.Println(x.IsASCII())
		fmt.Println(normalize(s))

	}
	{
		s := "hai.co.jp/&form=PRFUJ1&src=IE11TR&pc=FSTE"
		x := utf8string.NewString(s)
		fmt.Println(x.IsASCII())
		fmt.Println(normalize(s))

	}
	{
		fmt.Println(
			"hai.co.jp/…ge,sp/slidervideo,video,gmocloud,thm.jpg",
			"hai.co.jp/&form=PRFUJ1&src=IE11TR&pc=FSTE",
			normalize("hai.co.jp/…ge,sp/slidervideo,video,gmocloud,thm.jpg"),
			normalize("hai.co.jp/&form=PRFUJ1&src=IE11TR&pc=FSTE"),
			normalize("hai.co.jp/…ge,sp/slidervideo,video,gmocloud,thm.jpg") < normalize("hai.co.jp/&form=PRFUJ1&src=IE11TR&pc=FSTE"),
		)
	}
}
