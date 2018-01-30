package internal

import "go/types"

type protector struct {
	imp types.Importer
}

func (p *protector) Import(path string) (pkg *types.Package, err error) {
	defer func() {
		if recover() != nil {
			pkg = nil
			err = importFailed
		}
	}()
	return p.imp.Import(path)
}

// protect protects an importer imp from panics and returns the protected importer.
func protect(imp types.Importer) types.Importer {
	return &protector{imp}
}
