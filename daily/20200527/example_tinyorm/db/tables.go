package db

import "m/miniq"

var Book = struct {
	miniq.Table
	BookID    miniq.Int64Field
	Published miniq.Int64Field
	Title     miniq.StringField
	URL       miniq.StringField
}{
	Table:     miniq.Table("Book"),
	BookID:    miniq.Int64Field("bookId"),
	Published: miniq.Int64Field("published"),
	Title:     miniq.StringField("title"),
	URL:       miniq.StringField("url"),
}

var (
	Select = Book.Select
	Where  = Book.Where
)

var (
	Literalf = miniq.Literalf
	Count    = miniq.Count
	STAR     = miniq.STAR
)
