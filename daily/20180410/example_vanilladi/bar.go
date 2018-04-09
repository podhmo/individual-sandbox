package vanilladi

// Bar :
type Bar interface {
	DoSomething(value int)
}

type barNull struct {
}

func (b *barNull) DoSomething(value int) {
}

// NewBarNull :
func NewBarNull() Bar {
	return &barNull{}
}
