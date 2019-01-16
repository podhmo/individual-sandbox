```console
$ usage: app [<flags>] <command> [<args> ...]

sub command examples

Flags:
  --help  Show context-sensitive help (also try --help-long and --help-man).

Commands:
  help [<command>...]
    Show help.

  add [<ns>...]
    add numbers

  hello [<flags>] [<name>]
    hello message

$ go run main.go hello
hello
$ go run main.go hello -v
(john doe) Hello
$ go run main.go hello -v foo
(foo) Hello

$ go run main.go add 1000 100
{false [1100]} = 1000 + 100
$ go run main.go add 1000000000000000000 1000000000000000000 1000000000000000000 1000000000000000000 1000000000000000000 1000000000000000000 1000000000000000000 1000000000000000000 1000000000000000000 1000000000000000000
{false [10000000000000000000]} = 1000000000000000000 + 1000000000000000000 + 1000000000000000000 + 1000000000000000000 + 1000000000000000000 + 1000000000000000000 + 1000000000000000000 + 1000000000000000000 + 1000000000000000000 + 1000000000000000000
```
