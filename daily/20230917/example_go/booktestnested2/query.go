package booktestnested2

import (
	"context"

	"github.com/lib/pq"
)

type BookQueries struct {
	db DBTX
}

func (q *Queries) Book() *BookQueries {
	return &BookQueries{db: q.db}
}

const booksByTags = `-- name: BooksByTags :many
SELECT 
  book_id,
	  title,
  name,
  isbn,
  tags
FROM books
LEFT JOIN authors ON books.author_id = authors.author_id
WHERE tags && $1::varchar[]
`

func (q *BookQueries) BooksByTags(ctx context.Context, dollar_1 []string) ([]BooksByTagsRow, error) {
	rows, err := q.db.QueryContext(ctx, booksByTags, pq.Array(dollar_1))
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var items []BooksByTagsRow
	for rows.Next() {
		var i BooksByTagsRow
		if err := rows.Scan(
			&i.BookID,
			&i.Title,
			&i.Name,
			&i.Isbn,
			pq.Array(&i.Tags),
		); err != nil {
			return nil, err
		}
		items = append(items, i)
	}
	if err := rows.Close(); err != nil {
		return nil, err
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}
	return items, nil
}

const booksByTitleYear = `-- name: BooksByTitleYear :many
SELECT book_id, author_id, isbn, book_type, title, year, available, tags FROM books
WHERE title = $1 AND year = $2
`

func (q *BookQueries) BooksByTitleYear(ctx context.Context, modify func(arg *BooksByTitleYearParams)) ([]Book, error) {
	var arg BooksByTitleYearParams
	modify(&arg)

	rows, err := q.db.QueryContext(ctx, booksByTitleYear, arg.Title, arg.Year)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var items []Book
	for rows.Next() {
		var i Book
		if err := rows.Scan(
			&i.BookID,
			&i.AuthorID,
			&i.Isbn,
			&i.BookType,
			&i.Title,
			&i.Year,
			&i.Available,
			pq.Array(&i.Tags),
		); err != nil {
			return nil, err
		}
		items = append(items, i)
	}
	if err := rows.Close(); err != nil {
		return nil, err
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}
	return items, nil
}

type AuthorQueries struct {
	db DBTX
}

func (q *Queries) Author() *AuthorQueries {
	return &AuthorQueries{db: q.db}
}

const createAuthor = `-- name: CreateAuthor :one
INSERT INTO authors (name) VALUES ($1)
RETURNING author_id, name
`

func (q *AuthorQueries) CreateAuthor(ctx context.Context, name string) (Author, error) {
	row := q.db.QueryRowContext(ctx, createAuthor, name)
	var i Author
	err := row.Scan(&i.AuthorID, &i.Name)
	return i, err
}

const createBook = `-- name: CreateBook :one
INSERT INTO books (
    author_id,
    isbn,
    book_type,
    title,
    year,
    available,
    tags
) VALUES (
    $1,
    $2,
    $3,
    $4,
    $5,
    $6,
    $7
)
RETURNING book_id, author_id, isbn, book_type, title, year, available, tags
`

func (q *BookQueries) CreateBook(ctx context.Context, modify func(arg *CreateBookParams)) (Book, error) {
	var arg CreateBookParams
	modify(&arg)

	row := q.db.QueryRowContext(ctx, createBook,
		arg.AuthorID,
		arg.Isbn,
		arg.BookType,
		arg.Title,
		arg.Year,
		arg.Available,
		pq.Array(arg.Tags),
	)
	var i Book
	err := row.Scan(
		&i.BookID,
		&i.AuthorID,
		&i.Isbn,
		&i.BookType,
		&i.Title,
		&i.Year,
		&i.Available,
		pq.Array(&i.Tags),
	)
	return i, err
}

const deleteBook = `-- name: DeleteBook :exec
DELETE FROM books
WHERE book_id = $1
`

func (q *BookQueries) DeleteBook(ctx context.Context, bookID int32) error {
	_, err := q.db.ExecContext(ctx, deleteBook, bookID)
	return err
}

const getAuthor = `-- name: GetAuthor :one
SELECT author_id, name FROM authors
WHERE author_id = $1
`

func (q *AuthorQueries) GetAuthor(ctx context.Context, authorID int32) (Author, error) {
	row := q.db.QueryRowContext(ctx, getAuthor, authorID)
	var i Author
	err := row.Scan(&i.AuthorID, &i.Name)
	return i, err
}

const getBook = `-- name: GetBook :one
SELECT book_id, author_id, isbn, book_type, title, year, available, tags FROM books
WHERE book_id = $1
`

func (q *BookQueries) GetBook(ctx context.Context, bookID int32) (Book, error) {
	row := q.db.QueryRowContext(ctx, getBook, bookID)
	var i Book
	err := row.Scan(
		&i.BookID,
		&i.AuthorID,
		&i.Isbn,
		&i.BookType,
		&i.Title,
		&i.Year,
		&i.Available,
		pq.Array(&i.Tags),
	)
	return i, err
}

const updateBook = `-- name: UpdateBook :exec
UPDATE books
SET title = $1, tags = $2
WHERE book_id = $3
`

func (q *BookQueries) UpdateBook(ctx context.Context, modify func(arg *UpdateBookParams)) error {
	var arg UpdateBookParams
	modify(&arg)
	_, err := q.db.ExecContext(ctx, updateBook, arg.Title, pq.Array(arg.Tags), arg.BookID)
	return err
}

const updateBookISBN = `-- name: UpdateBookISBN :exec
UPDATE books
SET title = $1, tags = $2, isbn = $4
WHERE book_id = $3
`

func (q *BookQueries) UpdateBookISBN(ctx context.Context, modify func(arg *UpdateBookISBNParams)) error {
	var arg UpdateBookISBNParams
	modify(&arg)
	_, err := q.db.ExecContext(ctx, updateBookISBN,
		arg.Title,
		pq.Array(arg.Tags),
		arg.BookID,
		arg.Isbn,
	)
	return err
}
