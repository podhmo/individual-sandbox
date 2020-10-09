package main

import (
	"fmt"
	"go/ast"
	"log"
	"m/commentlookup"
)

// F this is F
// hello this is internal
func F() {}

// G this is G
func G() {}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
func run() error {
	l := commentlookup.NewLookup()
	{
		text, err := l.LookupCommentTextFromFunc(F)
		if err != nil {
			return err
		}
		fmt.Println(text)
	}
	return nil
}

func ShowCommentGroup(cg *ast.CommentGroup) {
	if cg == nil {
		return
	}
	fmt.Printf("	cg	%v	%v\n", cg.Pos(), cg.End())
	for _, c := range cg.List {
		fmt.Printf("		c	%v	%q\n", c.Slash, c.Text)
	}
}
