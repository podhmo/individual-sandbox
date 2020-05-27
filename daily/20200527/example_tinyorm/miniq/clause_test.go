package miniq

import (
	"fmt"
	"testing"

	rfc3339 "github.com/podhmo/go-rfc3339"
)

type Book struct {
	BookID    int64  `db:"bookId,primarykey,autoincrement"`
	Published int64  `db:"published"`
	Title     string `db:"title,size:140"` // Column size set to 140
	URL       string `db:"url,size:1024"`  // Set both column name and size
}

var (
	BookID    = Int64Field("bookId")
	Published = Int64Field("published")
	Title     = StringField("title")
	URL       = StringField("url")
)

func TestWhere(t *testing.T) {
	b := Book{
		BookID:    1,
		URL:       "http://example.net",
		Published: rfc3339.MustParse("2000-01-01T00:00:00Z").UnixNano(),
	}

	cases := []struct {
		input interface{}
		want  string
	}{
		{
			input: Where(
				BookID.Compare("= ?", b.BookID),
			),
			want: "WHERE bookId = ? ",
		},
		{
			input: Where(
				BookID.Compare("= ?", b.BookID),
				URL.Compare("= ?", b.URL),
				Published.Compare("> ?", b.Published),
			),
			want: "WHERE bookId = ?  AND url = ?  AND published > ? ",
		},
		{
			input: Where(
				BookID.Compare("= ?", b.BookID),
				Or(
					URL.Compare("= ?", b.URL),
					Published.Compare("> ?", b.Published),
				),
			),
			want: "WHERE bookId = ?  AND (url = ?  OR published > ? )",
		},
	}

	for i, c := range cases {
		c := c
		t.Run(fmt.Sprintf("case:%d", i), func(t *testing.T) {
			got := fmt.Sprintf("%+v", c.input)
			if c.want != got {
				t.Errorf("\nwant\n\t%v\nbut\n\t%s", c.want, got)
			}
		})
	}
}

func TestSelect(t *testing.T) {
	cases := []struct {
		input interface{}
		want  string
	}{
		{
			input: Select(STAR),
			want:  "SELECT *",
		},
		{
			input: Select(BookID),
			want:  "SELECT bookId",
		},
		{
			input: Select(BookID, Title),
			want:  "SELECT bookId, title",
		},
		{
			input: Select(BookID.As("id")),
			want:  "SELECT bookId as id",
		},
		{
			input: Select(Count(STAR)),
			want:  "SELECT COUNT(*)",
		},
		{
			input: Select(
				BookID.As("id"),
				Literalf("case when %s %% 2 = 0 then 1 else 0 end as even", BookID),
				Literalf("case when %s %% 2 = 1 then 1 else 0 end", BookID).As("odd"),
			),
			want: "SELECT bookId as id, case when bookId % 2 = 0 then 1 else 0 end as even, case when bookId % 2 = 1 then 1 else 0 end as odd",
		},
	}

	for i, c := range cases {
		c := c
		t.Run(fmt.Sprintf("case:%d", i), func(t *testing.T) {
			got := fmt.Sprintf("%v", c.input)
			if c.want != got {
				t.Errorf("\nwant\n\t%v\nbut\n\t%s", c.want, got)
			}
		})
	}
}
