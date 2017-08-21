package main

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/k0kubun/pp"
	"github.com/sergi/go-diff/diffmatchpatch"
)

// Diff finds the differences between two variable.
func Diff(a, b interface{}) string {
	old := pp.ColoringEnabled
	pp.ColoringEnabled = false
	sa := pp.Sprint(a)
	sb := pp.Sprint(b)
	pp.ColoringEnabled = old

	dmp := diffmatchpatch.New()
	res1, res2, res3 := dmp.DiffLinesToChars(sa, sb)
	diffs := dmp.DiffCharsToLines(dmp.DiffMain(res1, res2, false), res3)

	var result bytes.Buffer
	for _, diff := range diffs {
		switch diff.Type {
		case diffmatchpatch.DiffInsert:
			_, _ = result.WriteString("\n+ ")
			s := strings.Split(diff.Text, "\n")
			_, _ = result.WriteString(strings.Join(s[:len(s)-1], "\n+ "))
			_, _ = result.WriteString("\n")
		case diffmatchpatch.DiffDelete:
			_, _ = result.WriteString("\n- ")
			s := strings.Split(diff.Text, "\n")
			_, _ = result.WriteString(strings.Join(s[:len(s)-1], "\n- "))
			_, _ = result.WriteString("\n")
		}
	}
	return result.String()
}

func main() {
	m0 := map[string]int{
		"not found": 404,
		"error":     500,
	}
	m1 := map[string]int{
		"ok":         200,
		"created":    201,
		"no content": 204,
	}
	mm := map[string]map[string]int{
		"a": m0,
		"b": m1,
	}

	for i := 0; i < 5; i++ {
		fmt.Println(Diff(mm, mm))
	}
}
