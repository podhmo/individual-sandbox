package db

import "m/miniq"

type BookDefinition struct {
	miniq.Table
	BookID    miniq.Int64Field
	Published miniq.Int64Field
	Title     miniq.StringField
	URL       miniq.StringField
}

// TODO: remove, todo: cached
func (p *BookDefinition) As(name string) BookDefinition {
	new := *p // copy
	Alias(&new, p, name)
	return new
}

var Book = BookDefinition{
	Table:     miniq.Table("Book"),
	BookID:    miniq.Int64Field("bookId"),
	Published: miniq.Int64Field("published"),
	Title:     miniq.StringField("title"),
	URL:       miniq.StringField("url"),
}

type PostDefinition struct {
	miniq.Table
	PostID    miniq.Int64Field
	Published miniq.Int64Field
	Title     miniq.StringField
	URL       miniq.StringField
}

// TODO: remove, todo: cached
func (p *PostDefinition) As(name string) PostDefinition {
	new := *p // copy
	Alias(&new, p, name)
	return new
}

var Post = PostDefinition{
	Table:     miniq.Table("Post"),
	PostID:    miniq.Int64Field("postId"),
	Published: miniq.Int64Field("published"),
	Title:     miniq.StringField("title"),
	URL:       miniq.StringField("url"),
}

type CommentDefinition struct {
	miniq.Table
	CommentID miniq.Int64Field
	PostID    miniq.Int64Field
	Content   miniq.StringField
}

func (d *CommentDefinition) As(name string) *CommentDefinition {
	new := *d
	miniq.Alias(&new, d, name) // todo: cache?
	return &new
}

var Comment = &CommentDefinition{
	Table:     miniq.Table("Comment"),
	CommentID: miniq.Int64Field("commentId"),
	PostID:    miniq.Int64Field("postId"),
	Content:   miniq.StringField("content"),
}

var (
	Select = Book.Select
	From   = Book.From
	Where  = Book.Where
	Query  = miniq.Query
	On     = miniq.On
)

var (
	Literalf = miniq.Literalf
	Count    = miniq.Count
	STAR     = miniq.STAR
)
