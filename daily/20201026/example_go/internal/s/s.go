package s

type S struct{}

func (s *S) M() {}
func (s *S) m() {}

func F() {}
func f() {}

var (
	G = func() {}
	g = func() {}
)

func H()
func h()
