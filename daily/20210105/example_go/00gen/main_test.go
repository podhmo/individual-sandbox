package main

import (
	"io/ioutil"
	"testing"
)

func TestRun(t *testing.T) {
	d := t.TempDir()
	t.Logf("outdir: %s", d)
	if err := Run(d); err != nil {
		t.Errorf("!! %+v", err)
	}

	files, err := ioutil.ReadDir(d)
	if err != nil {
		t.Errorf("!!! %+v", err)
	}

	wantFiles := []string{"1.txt", "2.txt", "3.txt"}
	if len(files) != len(wantFiles) {
		t.Errorf("mismatch the number of files, got=%d, want=%d", len(files), len(wantFiles))
	}

	for i, f := range files {
		if f.Name() != wantFiles[i] {
			t.Errorf("files[%d], %s != %s", i, f.Name(), wantFiles[i])
		}
	}
}
