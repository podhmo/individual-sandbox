package deps

import (
	"../accessing"
	"../idgen"
)

// NewDummyStateWalker is x
func NewDummyStateWalker(settings *ServiceSettings) *StateWalker {
	dummyAccessor := accessing.DummyAccessor{}
	sw := StateWalker{
		Settings: settings,
		gen:      idgen.NewOnMemoryObjectIDGenerator(),
		accessor: &dummyAccessor,
	}
	return &sw
}
