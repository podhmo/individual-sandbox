package internal

import (
	"errors"
	"fmt"
	"go/types"
)

// lists of registered sources and corresponding importers
var (
	sources      []string
	importers    []types.Importer
	importFailed = errors.New("import failed")
)

// TryImporters is an importer that tries all registered importers
// successively until one of them succeeds or all of them failed.
type TryImporters struct{}

// Import :
func (t *TryImporters) Import(path string) (pkg *types.Package, err error) {
	for i, imp := range importers {
		fmt.Printf("\t\ttrying %q import\n", sources[i])
		pkg, err = imp.Import(path)
		if err == nil {
			break
		}
		fmt.Printf("\t\t=> %q import failed: %s\n", sources[i], err)
	}
	return
}
