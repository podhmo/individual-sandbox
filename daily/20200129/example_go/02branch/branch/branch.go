package branch

type XType string

const (
	XTypeFoo = XType("foo")
	XTypeBar = XType("bar")
	XTypeBoo = XType("boo")
)

// Command ...
type Command struct {
	xxx  func(x XType) error
	Cont func(x XType) error
}

// Do ...
func (cmd *Command) Do(x XType) error {
	switch x {
	case XTypeFoo:
		return nil
	case XTypeBar:
		return nil
	case XTypeBoo:
		return cmd.Cont(x)
	default:
		panic("something wrong")
	}
}

// New ...
func New() *Command {
	return &Command{
		Cont: func(x XType) error {
			panic("long long process, accessing external resource")
		},
	}
}
