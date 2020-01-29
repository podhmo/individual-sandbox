package news

import (
	"fmt"
	"io"
	"strings"
	"testing"
)

func Use(title string, news News, w io.Writer) error {
	fmt.Fprintf(w, "# %s\n", title)
	fmt.Fprintln(w, "")

	titles, err := news.Get()
	if err != nil {
		return err
	}
	for _, title := range titles {
		fmt.Fprintf(w, "- %s\n", title)
	}
	return nil
}

func TestIt(t *testing.T) {
	news := &FuncNews{
		Get: func() ([]string, error) {
			return []string{
				"foo",
				"bar",
				"boo",
			}, nil
		},
	}

	var b strings.Builder
	if err := Use("my news", news, &b); err != nil {
		t.Fatalf("something wrong: %s", err)
	}

	want := `# my news

- foo
- bar
- boo
`
	got := b.String()
	if want != got {
		t.Errorf("want %s, but %s", want, got)
	}
}
