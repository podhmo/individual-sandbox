package booktestfunc

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

func BooksByTags(ctx context.Context, dollar_1 []string) ([]BooksByTagsRow, error) {
	return nil, nil
}

type BooksByTitleYearParams struct {
	Title string
	Year  int32
}

func BooksByTitleYear(ctx context.Context, arg BooksByTitleYearParams) ([]schema.Book, error) {
	return nil, nil
}

func CreateAuthor(ctx context.Context, name string) (schema.Author, error) {
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

func CreateBook(ctx context.Context, arg CreateBookParams) (schema.Book, error) {
	return schema.Book{}, nil
}

func DeleteBook(ctx context.Context, bookID int32) error {
	return nil
}

func GetAuthor(ctx context.Context, authorID int32) (schema.Author, error) {
	return schema.Author{}, nil
}

func GetBook(ctx context.Context, bookID int32) (schema.Book, error) {
	return schema.Book{}, nil
}

type UpdateBookParams struct {
	Title  string
	Tags   []string
	BookID int32
}

func UpdateBook(ctx context.Context, arg UpdateBookParams) error {
	return nil
}

type UpdateBookISBNParams struct {
	Title  string
	Tags   []string
	BookID int32
	Isbn   string
}

func UpdateBookISBN(ctx context.Context, arg UpdateBookISBNParams) error {
	return nil
}
