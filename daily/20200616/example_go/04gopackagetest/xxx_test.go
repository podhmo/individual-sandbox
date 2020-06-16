package xxx

import (
	"os/exec"
	"testing"

	"golang.org/x/tools/go/packages/packagestest"
)

// TestGoList exercises the 'go list' command in module mode and in GOPATH mode.
func TestGoList(t *testing.T) { packagestest.TestAll(t, testGoList) }
func testGoList(t *testing.T, x packagestest.Exporter) {
	e := packagestest.Export(t, x, []packagestest.Module{
		{
			Name: "gopher.example/repoa",
			Files: map[string]interface{}{
				"a/a.go": "package a",
			},
		},
		{
			Name: "gopher.example/repob",
			Files: map[string]interface{}{
				"b/b.go": "package b",
			},
		},
	})
	defer e.Cleanup()

	cmd := exec.Command("go", "list", "gopher.example/...")
	cmd.Dir = e.Config.Dir
	cmd.Env = e.Config.Env
	out, err := cmd.Output()
	if err != nil {
		t.Fatal(err)
	}
	t.Logf("'go list gopher.example/...' with %s mode layout:\n%s", x.Name(), out)
}
