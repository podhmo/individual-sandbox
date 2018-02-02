package main

import (
	"fmt"
	"go/build"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"golang.org/x/tools/go/buildutil"
)

func main() {
	ctxt := build.Default
	sep := string(filepath.Separator)

	srcpath := filepath.Clean(filepath.Join(ctxt.GOPATH, "src"))
	candidates := []string{ctxt.GOPATH, srcpath}
	var buf []string

	pkg := "github.com/podhmo/sandbox"

	names := strings.Split(pkg, sep)
	for i := 0; i < len(names); i++ {
		if names[i] == "" {
			continue
		}
		buf = append(buf, names[i])
		candidates = append(candidates, filepath.Join(srcpath, strings.Join(buf, sep)))
	}
	ctxt.ReadDir = func(path string) ([]os.FileInfo, error) {
		size := len(path)
		for i := range candidates {
			if size > len(candidates[i]) && !strings.HasPrefix(path, candidates[i]) {
				return nil, nil
			}
		}

		if strings.Contains(path, "/vendor") {
			return nil, nil
		}
		if strings.Contains(path, "/swagger") {
			return nil, nil
		}
		if strings.Contains(path, "/proposalanalyzer") {
			return nil, nil
		}
		return ioutil.ReadDir(path)
	}
	buildutil.ForEachPackage(&ctxt, func(path string, err error) {
		fmt.Println(path)
	})
}
