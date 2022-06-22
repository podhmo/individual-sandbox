package main

type Action[InputT any, OutputT any] func(InputT) (OutputT, error)

func lift[InputT any, OutputT any](action Action[InputT, OutputT]) {
	// Bindがいる
}

func main(){
}
