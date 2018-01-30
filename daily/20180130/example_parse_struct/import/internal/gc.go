package internal

import "go/importer"

func init() {
	register("gc", importer.For("gc", nil))
}
