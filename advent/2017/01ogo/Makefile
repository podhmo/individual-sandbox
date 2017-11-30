build:
	(cd go/src/; GOROOT_BOOTSTRAP=`go env | grep GOROOT | cut -d = -f 2` bash -x make.bash --no-clean)

build-with-clean:
	(cd go/src/; GOROOT_BOOTSTRAP=`go env | grep GOROOT | cut -d = -f 2` bash -x make.bash)

run:
	go/bin/go run 01*/main.go

clone:
	git clone --depth=1 git@github.com:golang/go.git

sampling:
	rm -f sampling.original sampling.changed
	for i in `seq 100`; do go run 01iterate/main.go | grep -oP '^\d+' | tr '\n' ' ' >> sampling.original;  printf '\n' >> sampling.original; done
	for i in `seq 100`; do go/bin/go run 01iterate/main.go | grep -v @@ | grep -oP '^\d+' | tr '\n' ' ' >> sampling.changed;  printf '\n' >> sampling.changed;  done
