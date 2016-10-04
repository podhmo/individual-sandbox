package deps


// NewDummyStateWalker is x
func NewDummyStateWalker(settings *ServiceSettings) *StateWalker {
	dummyAccessor := DummyAccessor{}
	sw := StateWalker{
		Settings: settings,
		gen:      NewOnMemoryObjectIDGenerator(),
		accessor: &dummyAccessor,
	}
	return &sw
}

