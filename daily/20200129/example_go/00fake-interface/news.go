package news

// FuncNews ...
type FuncNews struct {
	Get func() ([]string, error)
}

// News ...
type News *FuncNews
