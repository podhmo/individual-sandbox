setup:
	go get -v github.com/google/go-jsonnet/jsonnet
	go get -v github.com/google/go-jsonnet/linter/jsonnet-lint

sample:
	jsonnet /dev/stdin <<< '{x: 1, y: self.x} + {x: 10}'

