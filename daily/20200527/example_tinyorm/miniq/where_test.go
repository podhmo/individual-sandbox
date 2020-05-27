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

func TestWhere(t *testing.T) {
	BookID := NewInt64Field("bookId")
	Published := NewInt64Field("published")
	URL := NewStringField("url")

	b := Book{
		BookID:    1,
		URL:       "http://example.net",
		Published: rfc3339.MustParse("2000-01-01T00:00:00Z").UnixNano(),
	}

	cases := []struct {
		where interface{}
		want  string
	}{
		{
			where: Where(
				BookID("%s = ?", b.BookID),
			),
			want: "WHERE bookId = ? ",
		},
		{
			where: Where(
				BookID("%s = ?", b.BookID),
				URL("%s = ?", b.URL),
				Published("%s > ?", b.Published),
			),
			want: "WHERE bookId = ?  AND url = ?  AND published > ? ",
		},
		{
			where: Where(
				BookID("%s = ?", b.BookID),
				Or(
					URL("%s = ?", b.URL),
					Published("%s > ?", b.Published),
				),
			),
			want: "WHERE bookId = ?  AND (url = ?  OR published > ? )",
		},
	}

	for i, c := range cases {
		c := c
		t.Run(fmt.Sprintf("case:%d", i), func(t *testing.T) {
			got := fmt.Sprintf("%+v", c.where)
			if c.want != got {
				t.Errorf("\nwant\n\t%v\nbut\n\t%s", c.want, got)
			}
		})
	}
}
