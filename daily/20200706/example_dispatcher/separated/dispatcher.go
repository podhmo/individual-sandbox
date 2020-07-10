package separated

type Dispatcher interface {
	XXXDispatcher
	YYYDispatcher
}

type XXXDispatcher interface {
	OnXXX(name string)
}

type YYYDispatcher interface {
	OnYYY(name string)
}
