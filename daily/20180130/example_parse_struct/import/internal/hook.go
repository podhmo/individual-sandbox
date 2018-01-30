package internal

import "go/types"

// register registers an importer imp for a given source src.
func register(src string, imp types.Importer) {
	if lookup(src) != nil {
		panic(src + " importer already registered")
	}
	sources = append(sources, src)
	importers = append(importers, protect(imp))
}

// lookup returns the importer imp for a given source src.
func lookup(src string) types.Importer {
	for i, s := range sources {
		if s == src {
			return importers[i]
		}
	}
	return nil
}
