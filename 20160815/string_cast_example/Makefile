default:
	make zero
	make one
	make diff

clean:
	rm -f *.asm *.diff

zero:
	go run 00-hello.go
	go tool compile -S 00-hello.go > 00-hello.asm
	go tool compile -N -S 00-hello.go > 00-hello-deoptimize.asm

one:
	go run 01-hello.go
	go tool compile -S 01-hello.go > 01-hello.asm
	go tool compile -N -S 01-hello.go > 01-hello-deoptimize.asm

diff:
	cat 00-hello.asm | ruby -ne '$$_ =~ /^\t.+?\t(.+)/ && puts($$1)' > 00-hello-simple.asm
	cat 01-hello.asm | ruby -ne '$$_ =~ /^\t.+?\t(.+)/ && puts($$1)' > 01-hello-simple.asm
	cat 00-hello-deoptimize.asm | ruby -ne '$$_ =~ /^\t.+?\t(.+)/ && puts($$1)' > 00-hello-simple-deoptimize.asm
	cat 01-hello-deoptimize.asm | ruby -ne '$$_ =~ /^\t.+?\t(.+)/ && puts($$1)' > 01-hello-simple-deoptimize.asm
	diff -u 00-hello-simple.asm 01-hello-simple.asm > 00-01.diff || echo ok
	diff -u 00-hello-simple-deoptimize.asm 01-hello-simple-deoptimize.asm > 00-01-deoptimize.diff || echo ok

.PHONY: zero one diff
