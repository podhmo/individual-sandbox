package types

import (
	"database/sql"
	"time"
)

type BooksByTagsRow struct {
	BookID int32
	Title  string
	Name   sql.NullString
	Isbn   string
	Tags   []string
}

type BooksByTitleYearParams struct {
	Title string
	Year  int32
}

type CreateBookParams struct {
	AuthorID  int32
	Isbn      string
	BookType  BookType
	Title     string
	Year      int32
	Available time.Time
	Tags      []string
}

type UpdateBookParams struct {
	Title  string
	Tags   []string
	BookID int32
}

type UpdateBookISBNParams struct {
	Title  string
	Tags   []string
	BookID int32
	Isbn   string
}
