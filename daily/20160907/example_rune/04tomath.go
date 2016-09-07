package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
)

func toMathGen(rx *regexp.Regexp, ra, rindex rune) func(string) string {
	return func(s string) string {
		return rx.ReplaceAllStringFunc(s, func(x string) string {
			runes := []rune(x)
			for i, ri := range runes {
				runes[i] = rindex + (ri - ra)
			}
			return string(runes)
		})
	}
}

func main() {
	// https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
	lrx := regexp.MustCompile("[a-z]")
	toMathLower := toMathGen(lrx, rune('a'), rune(0x1D4EA))
	urx := regexp.MustCompile("[A-Z]")
	toMathUpper := toMathGen(urx, rune('A'), rune(0x1D4D0))

	if len(os.Args) <= 1 {
		buf, err := ioutil.ReadAll(os.Stdin)
		if err != nil {
			panic(err)
		}
		fmt.Print(toMathLower(toMathUpper(string(buf))))
	} else {
		for _, word := range os.Args[1:] {
			fmt.Print(toMathLower(toMathUpper(word)))
		}
	}
}
