package main

import (
	"fmt"
	"log"
	"regexp"
	"strings"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

var rx = regexp.MustCompile(`(?:^|\s*)(@+[a-zA-Z_\-]+)\s*(?:=|,|$)`)

func Lex(s string) [][2]string {
	// BUG: not support like this. `@pattern=\\d+@xxx=+=$,@unique`, (work-around is `@pattern=\\d+[@]xxx=+=$,@unique` )

	// <predicate> :: { <attribute> '=' <args> }+
	// <attribute> :: { '@' }+ { <token> }+
	// <args> :: { <token> ','}* <token>

	indexList := rx.FindAllStringSubmatchIndex(s, -1)
	if len(indexList) == 0 {
		return nil
	}
	r := make([][2]string, 0, len(indexList))
	n := len(indexList)
	for i := 1; i < n; i++ {
		index := indexList[i-1]
		r = append(r, [2]string{
			s[index[2]:index[3]],
			strings.TrimSpace(strings.TrimSuffix(s[index[1]:indexList[i][0]], ",")),
		})
	}
	index := indexList[n-1]
	r = append(r, [2]string{
		s[index[2]:index[3]],
		strings.TrimSpace(s[index[1]:]),
	})
	return r
}

func run() error {
	xs := []string{
		`@pattern=[A-Z].*`,
		`@pattern=[A-Z].*,@notzero`,
		`@notzero,@pattern=[A-Z].*`,
		`@pattern=[A-Z].* , @notzero `,
		`@min-length=1,@pattern=\\d+$,@unique`,
		`@@positive=,@max-length=99`,
		`@min-length=1,@pattern=\\d+$,@unique`,
		`@min-length=1,@pattern=\\d+@xxx=+=$,@unique`,
		`@min-length=1,@pattern=\\d+[@]xxx=+=$,@unique`,
		`@hmm`,
		`@pattern=(\\S+,\\S+),@notzero`,
	}
	for _, x := range xs {
		fmt.Printf("%q	%q\n", x, Lex(x))
	}
	return nil
}
