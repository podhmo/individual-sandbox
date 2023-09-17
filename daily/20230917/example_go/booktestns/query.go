package booktestns

import (
	"context"
	"database/sql"
	"time"

	"github.com/podhmo/individual-sandbox/daily/20230917/example_go/internal/schema"
)

type bookNS struct{}

var Book = &bookNS{}

type BooksByTagsRow struct {
	BookID int32
	Title  string
	Name   sql.NullString
	Isbn   string
	Tags   []string
}

type booksByTagsParams struct {
	Dollar1 []string
}

func (ns *bookNS) BooksByTags() *booksByTagsParams {
	var z booksByTagsParams
	return &z
}

func (p *booksByTagsParams) Do(ctx context.Context) ([]BooksByTagsRow, error) {
	return nil, nil
}

type booksByTitleYearParams struct {
	Title string
	Year  int32
}

func (ns *bookNS) BooksByTitleYear() *booksByTitleYearParams {
	var z booksByTitleYearParams
	return &z
}

func (p *booksByTitleYearParams) Do(ctx context.Context) ([]schema.Book, error) {
	return nil, nil
}

type authorNS struct{}

var Author = &authorNS{}

type createAuthorParams struct {
	Name string
}

func (ns *authorNS) CreateAuthor() *createAuthorParams {
	var z createAuthorParams
	return &z
}

func (p *createAuthorParams) Do(ctx context.Context) (schema.Author, error) {
	return schema.Author{}, nil
}

type createBookParams struct {
	AuthorID  int32
	Isbn      string
	BookType  schema.BookType
	Title     string
	Year      int32
	Available time.Time
	Tags      []string
}

func (ns *bookNS) CreateBook() *createBookParams {
	var z createBookParams
	return &z
}
func (p *createBookParams) Do(ctx context.Context) (schema.Book, error) {
	return schema.Book{}, nil
}

type deleteBookParams struct {
	BookID int32
}

func (ns *bookNS) DeleteBook() *deleteBookParams {
	var z deleteBookParams
	return &z
}
func (p *deleteBookParams) Do(ctx context.Context) error {
	return nil
}

type getAuthorParams struct {
	AuthorID int32
}

func (ns *authorNS) GetAuthor() *getAuthorParams {
	var z getAuthorParams
	return &z
}
func (p *getAuthorParams) Do(ctx context.Context) (schema.Author, error) {
	return schema.Author{}, nil
}

type getBookParams struct {
	BookID int32
}

func (ns *bookNS) GetBook() *getBookParams {
	var z getBookParams
	return &z
}
func (p *getBookParams) Do(ctx context.Context) (schema.Book, error) {
	return schema.Book{}, nil
}

type updateBookParams struct {
	Title  string
	Tags   []string
	BookID int32
}

func (ns *bookNS) UpdateBook() *updateBookParams {
	var z updateBookParams
	return &z
}
func (p *updateBookParams) Do(ctx context.Context) error {
	return nil
}

type updateBookISBNParams struct {
	Title  string
	Tags   []string
	BookID int32
	Isbn   string
}

func (ns *bookNS) UpdateBookISBN(ctx context.Context) *updateBookISBNParams {
	var z updateBookISBNParams
	return &z
}

func (p *updateBookISBNParams) Do(ctx context.Context) error {
	return nil
}
