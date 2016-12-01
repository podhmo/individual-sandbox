package main

import "fmt"

func parse(s string, sep rune, escape rune) []string {
	chunks := [][]rune{}
	buf := []rune{}
	escaped := false

	for _, r := range s {
		switch r {
		case escape:
			if escaped {
				buf = append(buf, r)
			}
			escaped = !escaped
		case sep:
			if escaped {
				escaped = false
				buf = append(buf, r)
			} else {
				chunks = append(chunks, buf)
				buf = []rune{}
			}
		default:
			escaped = false
			buf = append(buf, r)
		}
	}
	if len(buf) > 0 {
		chunks = append(chunks, buf)
	}
	result := make([]string, len(chunks))
	for i, buf := range chunks {
		result[i] = string(buf)
	}
	return result
}

func main() {
	p := func(s string) []string { return parse(s, '|', '\\') }
	fmt.Printf("%#+v\n", p("foo bar"))
	fmt.Printf("%#+v\n", p("foo|bar"))
	fmt.Printf("%#+v\n", p("foo\\|bar"))
	fmt.Printf("%#+v\n", p("foo\\\\|bar"))
	fmt.Printf("%#+v\n", p("foo\a\\|bar"))
}
