package vanilladi

type barSpy struct {
	calledArgs []int
}

func (b *barSpy) DoSomething(value int) {
	b.calledArgs = append(b.calledArgs, value)
}

// NewBarSpy :
func NewBarSpy() Bar {
	return &barSpy{}
}
