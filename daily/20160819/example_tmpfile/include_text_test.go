package main

import (
	"bufio"
	"io/ioutil"
	"os"
	"testing"
)

func prepareFile(filename string, content string) (*os.File, error) {
	// setup
	fp, err := ioutil.TempFile(".", filename)
	defer fp.Close()
	if err != nil {
		return nil, err
	}

	w := bufio.NewWriter(fp)
	w.WriteString(content)
	w.Flush()
	return fp, nil
}

func TestIncludeText(t *testing.T) {
	// setup
	fp, err := prepareFile("testtext-", "hello")
	if err != nil {
		t.Error(err)
	}
	tmpName := fp.Name()

	// teardown
	defer os.Remove(tmpName)

	// test main
	// TODO: table driven test
	{
		status := includeText(tmpName, "hello")
		if !status {
			t.Errorf("F includeText %q %q", tmpName, "hello")
		}
	}
	{
		status := includeText(tmpName, "bye")
		if status {
			t.Errorf("F includeText %q %q", tmpName, "bye")
		}
	}
}
