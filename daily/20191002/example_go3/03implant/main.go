package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"os"
	"sort"
)

func main() {
	fmt.Println(run())
}

func run() error {
	{
		fset := token.NewFileSet()
		f, err := parser.ParseFile(fset, "f.go", code, parser.ParseComments)
		if err != nil {
			return err
		}
		f2, err := parser.ParseFile(fset, "f2.go", code2, parser.ParseComments)
		if err != nil {
			return err
		}

		// <comment>
		// <type> <body/>
		// </body>

		{
			fdecl := f.Decls[0].(*ast.FuncDecl)
			f.Comments = append(f.Comments, &ast.CommentGroup{
				List: []*ast.Comment{
					{
						Slash: fdecl.Type.Pos() - 1,
						Text:  "/* pasteS */",
					},
				},
			})
			f.Comments = append(f.Comments, &ast.CommentGroup{
				List: []*ast.Comment{
					{
						Slash: fdecl.Type.End() - 1,
						Text:  "/* pasteE */",
					},
				},
			})
			sort.Slice(f.Comments, func(i, j int) bool {
				return f.Comments[i].Pos() < f.Comments[j].Pos()
			})
			printer.Fprint(os.Stdout, fset, f)
		}
		{
			f := f2
			fdecl := f.Decls[0].(*ast.FuncDecl)
			f.Comments = append(f.Comments, &ast.CommentGroup{
				List: []*ast.Comment{
					{
						Slash: fdecl.Type.Pos() - 1,
						Text:  "/* copyS */",
					},
				},
			})
			f.Comments = append(f.Comments, &ast.CommentGroup{
				List: []*ast.Comment{
					{
						Slash: fdecl.Type.End() - 1,
						Text:  "/* copyE */",
					},
				},
			})
			sort.Slice(f.Comments, func(i, j int) bool {
				return f.Comments[i].Pos() < f.Comments[j].Pos()
			})
			printer.Fprint(os.Stdout, fset, f)
		}
	}

	fmt.Println("----------------------------------------")

	{
		fset := token.NewFileSet()
		f, err := parser.ParseFile(fset, "f.go", code, parser.ParseComments)
		if err != nil {
			return err
		}
		f2, err := parser.ParseFile(fset, "f2.go", code2, parser.ParseComments)
		if err != nil {
			return err
		}

		// <comment>
		// <type> <body/>
		// </body>

		{
			fdecl := f.Decls[0].(*ast.FuncDecl)
			f.Comments = append(f.Comments, &ast.CommentGroup{
				List: []*ast.Comment{
					{
						Slash: fdecl.Body.Pos() - 1,
						Text:  "/* pasteS */",
					},
				},
			})
			f.Comments = append(f.Comments, &ast.CommentGroup{
				List: []*ast.Comment{
					{
						Slash: fdecl.Body.End() - 1,
						Text:  "/* pasteE */",
					},
				},
			})
			sort.Slice(f.Comments, func(i, j int) bool {
				return f.Comments[i].Pos() < f.Comments[j].Pos()
			})
			printer.Fprint(os.Stdout, fset, f)
		}
		{
			f := f2
			fdecl := f.Decls[0].(*ast.FuncDecl)
			f.Comments = append(f.Comments, &ast.CommentGroup{
				List: []*ast.Comment{
					{
						Slash: fdecl.Body.Pos() - 1,
						Text:  "/* copyS */",
					},
				},
			})
			f.Comments = append(f.Comments, &ast.CommentGroup{
				List: []*ast.Comment{
					{
						Slash: fdecl.Body.End() - 1,
						Text:  "/* copyE */",
					},
				},
			})
			sort.Slice(f.Comments, func(i, j int) bool {
				return f.Comments[i].Pos() < f.Comments[j].Pos()
			})
			printer.Fprint(os.Stdout, fset, f)
		}
	}
	return nil
}

const code string = `
package main

// F :
// - xxx
func F(n int) string {
	// n == 0 F
	if n == 0 {
		// return F
		return "<zero>"
	}
	return fmt.Sprintf("<N%d>", n)
} // end F
`
const code2 string = `
package main

func F(n int, m int) string {
	// n == 0 F2
	if n == 0 {
		// return F2
		return "<zero>"
	}
	return fmt.Sprintf("<N%d>", n)
} // end F2
`
