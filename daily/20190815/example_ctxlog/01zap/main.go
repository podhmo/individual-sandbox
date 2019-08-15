package main

import "go.uber.org/zap"

func main() {
	l, err := zap.NewDevelopment()
	if err != nil {
		panic(err)
	}
	l.Sugar().With("x", "before", "y", "before").Infow("heh", "x", "after", "z", "after")
}
