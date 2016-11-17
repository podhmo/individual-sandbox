package hmm2

type S string

// Bower :
//go:generate mockgen -source src.go -destination ./testmock.go -package hmm2
type Bower interface {
	Bow(s string) S
}

func f(b Bower, s string) S {
	return b.Bow(s)
}
