package main

import (
	"context"
	"fmt"

	"github.com/podhmo/individual-sandbox/daily/20230917/example_go/booktest"
	"github.com/podhmo/individual-sandbox/daily/20230917/example_go/booktestfunc"
	"github.com/podhmo/individual-sandbox/daily/20230917/example_go/booktestnested"
	"github.com/podhmo/individual-sandbox/daily/20230917/example_go/booktestnested2"
	"github.com/podhmo/individual-sandbox/daily/20230917/example_go/booktestns"
	"github.com/podhmo/individual-sandbox/daily/20230917/example_go/booktestvar"
)

func main() {
	ctx := context.Background()
	var bookID int32 = 1

	// booktest
	{
		q := booktest.New(nil)
		{
			book, err := q.GetBook(ctx, bookID)
			fmt.Println(book, err)
		}
		{
			p := booktest.UpdateBookParams{
				BookID: bookID,
				Title:  "foo",
			}
			err := q.UpdateBook(ctx, p)
			fmt.Println(err)
		}
	}

	// booktestfunc
	{
		{
			book, err := booktestfunc.GetBook(ctx, bookID)
			fmt.Println(book, err)

		}
		{
			p := booktestfunc.UpdateBookParams{
				BookID: bookID,
				Title:  "foo",
			}
			err := booktestfunc.UpdateBook(ctx, p)
			fmt.Println(err)
		}
	}

	// booktestns
	{
		{
			p := booktestns.Book.GetBook()
			p.BookID = 1
			book, err := p.Do(ctx)
			fmt.Println(book, err)
		}
		{
			p := booktestns.Book.UpdateBook()
			p.BookID = bookID
			p.Title = "foo"
			err := p.Do(ctx)
			fmt.Println(err)
		}
	}

	// booktestvar
	{
		{
			book, err := booktestvar.Book.GetBook(ctx, bookID)
			fmt.Println(book, err)
		}
		{
			p := booktestvar.UpdateBookParams{
				BookID: bookID,
				Title:  "foo",
			}
			err := booktestvar.Book.UpdateBook(ctx, p)
			fmt.Println(err)
		}

	}

	// booktestnested
	{
		q := booktestnested.New(nil)
		{
			book, err := q.Book().GetBook(ctx, bookID)
			fmt.Println(book, err)
		}
		{
			p := booktestnested.UpdateBookParams{
				BookID: bookID,
				Title:  "foo",
			}
			err := q.Book().UpdateBook(ctx, p)
			fmt.Println(err)
		}

	}
	// booktestnested2
	{
		q := booktestnested2.New(nil)
		{
			book, err := q.Book().GetBook(ctx, bookID)
			fmt.Println(book, err)
		}
		{
			err := q.Book().UpdateBook(ctx, func(arg *booktestnested2.UpdateBookParams) {
				arg.BookID = bookID
				arg.Title = "foo"
			})
			fmt.Println(err)
		}
	}
}
