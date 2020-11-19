export NAME ?= replaced
export AGE ?= 10


00:
	go run $(shell echo $@*/)main.go -name foo -age 20
01:
	go run $(shell echo $@*/)main.go -name foo -age 20
02:
	go run $(shell echo $@*/)main.go -name foo -age 20

## OK
# go run 03subcommands/main.go issue create --title xxx
# go run 03subcommands/main.go --help
# go run 03subcommands/main.go issue --help
# go run 03subcommands/main.go issue create --help
# go run 03subcommands/main.go foo --title xxx
## NG
# go run 03subcommands/main.go
# go run 03subcommands/main.go issue delete
# go run 03subcommands/main.go issue create
03:
	go run $(shell echo $@*/)main.go issue create --title xxx
04:
	go run $(shell echo $@*/)main.go issue create --title xxx
