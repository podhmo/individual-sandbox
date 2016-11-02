# wrap many

```bash
go run 00*/*
error is occured:
hmm
with g
main.g
	./00wrap/main.go:13
main.f
	./00wrap/main.go:10
main.main
	./00wrap/main.go:20
runtime.main
	/opt/local/lib/go/src/runtime/proc.go:183
runtime.goexit
	/opt/local/lib/go/src/runtime/asm_amd64.s:2086
with f
main.f
	./00wrap/main.go:10
main.main
	./00wrap/main.go:20
runtime.main
	/opt/local/lib/go/src/runtime/proc.go:183
runtime.goexit
	/opt/local/lib/go/src/runtime/asm_amd64.s:2086
```

# wrap once

```bash
go run 01*/*
error is occured:
hmm
with g
main.g
	./01wrap-once/main.go:13
main.f
	./01wrap-once/main.go:10
main.main
	./01wrap-once/main.go:20
runtime.main
	/opt/local/lib/go/src/runtime/proc.go:183
runtime.goexit
	/opt/local/lib/go/src/runtime/asm_amd64.s:2086
with f
```

