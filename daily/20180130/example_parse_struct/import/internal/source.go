package internal

import "go/types"

func init() {
	register("source", sourceImporter{})
}

type sourceImporter struct{}

func (sourceImporter) Import(path string) (*types.Package, error) {
	panic("unimplemented")
}
