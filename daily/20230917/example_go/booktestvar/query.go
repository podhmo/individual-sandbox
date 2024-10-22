package booktestvar

import (
	"context"
	"database/sql"
	"time"

	"github.com/podhmo/individual-sandbox/daily/20230917/example_go/internal/schema"
)

type BooksByTagsRow struct {
	BookID int32
	Title  string
	Name   sql.NullString
	Isbn   string
	Tags   []string
}

func booksByTags(ctx context.Context, dollar_1 []string) ([]BooksByTagsRow, error) {
	return nil, nil
}

type BooksByTitleYearParams struct {
	Title string
	Year  int32
}

func booksByTitleYear(ctx context.Context, arg BooksByTitleYearParams) ([]schema.Book, error) {
	return nil, nil
}

func createAuthor(ctx context.Context, name string) (schema.Author, error) {
	return schema.Author{}, nil
}

type CreateBookParams struct {
	AuthorID  int32
	Isbn      string
	BookType  schema.BookType
	Title     string
	Year      int32
	Available time.Time
	Tags      []string
}

func createBook(ctx context.Context, arg CreateBookParams) (schema.Book, error) {
	return schema.Book{}, nil
}

func deleteBook(ctx context.Context, bookID int32) error {
	return nil
}

func getAuthor(ctx context.Context, authorID int32) (schema.Author, error) {
	return schema.Author{}, nil
}

func getBook(ctx context.Context, bookID int32) (schema.Book, error) {
	return schema.Book{}, nil
}

type UpdateBookParams struct {
	Title  string
	Tags   []string
	BookID int32
}

func updateBook(ctx context.Context, arg UpdateBookParams) error {
	return nil
}

type UpdateBookISBNParams struct {
	Title  string
	Tags   []string
	BookID int32
	Isbn   string
}

func updateBookISBN(ctx context.Context, arg UpdateBookISBNParams) error {
	return nil
}

var Book struct {
	UpdateBookISBN   func(ctx context.Context, arg UpdateBookISBNParams) error
	UpdateBook       func(ctx context.Context, arg UpdateBookParams) error
	GetBook          func(ctx context.Context, bookID int32) (schema.Book, error)
	DeleteBook       func(ctx context.Context, bookID int32) error
	CreateBook       func(ctx context.Context, arg CreateBookParams) (schema.Book, error)
	BooksByTitleYear func(ctx context.Context, arg BooksByTitleYearParams) ([]schema.Book, error)
	BooksByTags      func(ctx context.Context, dollar_1 []string) ([]BooksByTagsRow, error)
}
var Author struct {
	GetAuthor    func(ctx context.Context, authorID int32) (schema.Author, error)
	CreateAuthor func(ctx context.Context, name string) (schema.Author, error)
}

func init() {
	Book.UpdateBookISBN = updateBookISBN
	Book.UpdateBook = updateBook
	Book.GetBook = getBook
	Book.DeleteBook = deleteBook
	Book.CreateBook = createBook
	Book.BooksByTitleYear = booksByTitleYear
	Book.BooksByTags = booksByTags

	Author.GetAuthor = getAuthor
	Author.CreateAuthor = createAuthor
}
