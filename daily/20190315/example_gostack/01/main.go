package main

import (
	"log"
	"runtime"

	"github.com/k0kubun/pp"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

// Token :
type Token struct {
	Kind  string
	Value string
}

// Tokenize :
func Tokenize(s string) []Token {
	var r []Token
	var buf []rune
	kind := "header"
	for _, c := range s {
		switch c {
		case '\n':
			r = append(r, Token{Kind: kind, Value: string(buf)})
			buf = []rune{}
			kind = "symbol"
		case '\t':
			kind = "location"
		default:
			buf = append(buf, c)
		}
	}
	return r
}

// Symbols :
func Symbols(tokens []Token) []string {
	var r []string
	for _, tk := range tokens {
		if tk.Kind == "symbol" {
			r = append(r, tk.Value)
		}
	}
	return r
}

// Locations :
func Locations(tokens []Token) []string {
	var r []string
	for _, tk := range tokens {
		if tk.Kind == "location" {
			r = append(r, tk.Value)
		}
	}
	return r
}

func run() error {
	fn := func() {
		buf := make([]byte, 1024)
		runtime.Stack(buf, false)
		// pp.Println(Tokenize(string(buf)))
		pp.Println(Symbols(Tokenize(string(buf))))
		pp.Println(Locations(Tokenize(string(buf))))
	}
	fn()
	return nil
}
