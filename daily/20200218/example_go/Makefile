# flag
00:
	go run $(shell echo $@*/)main.go -name foo -n 10
	go run $(shell echo $@*/)main.go
	go run $(shell echo $@*/)main.go -h 2>&1 | tee $(subst /,,$(shell echo $@*/)).output

# kingpin
01:
	go run $(shell echo $@*/)main.go --name foo -n 10
	go run $(shell echo $@*/)main.go
	go run $(shell echo $@*/)main.go -h 2>&1 | tee $(subst /,,$(shell echo $@*/)).output

# flaggy
02:
	go run $(shell echo $@*/)main.go --name foo -n 10
	go run $(shell echo $@*/)main.go
	go run $(shell echo $@*/)main.go -h 2>&1 | tee $(subst /,,$(shell echo $@*/)).output

# python (argparse)
03:
	python $(shell echo $@*.py) --name foo -n 10
	python $(shell echo $@*.py)
	python $(shell echo $@*.py) -h | tee $(patsubst %.py,%.output,$(shell echo $@*.py))

# python (handofcats)
04:
	python $(shell echo $@*.py) --name foo -n 10
	python $(shell echo $@*.py)
	python $(shell echo $@*.py) -h | tee $(patsubst %.py,%.output,$(shell echo $@*.py))

# pflag
05:
	go run $(shell echo $@*/)main.go --name foo -- 10
	go run $(shell echo $@*/)main.go
	go run $(shell echo $@*/)main.go -h 2>&1 | tee $(subst /,,$(shell echo $@*/)).output

# pflag
06:
	go run $(shell echo $@*/)main.go --name foo -- 10
	go run $(shell echo $@*/)main.go
	go run $(shell echo $@*/)main.go -h 2>&1 | tee $(subst /,,$(shell echo $@*/)).output

# subcommand pflag
10:
	go run $(shell echo $@*/)main.go -h
	go run $(shell echo $@*/)main.go

# subcommand flag
11:
	go run $(shell echo $@*/)main.go add -h
	go run $(shell echo $@*/)main.go -h
