# golang 一度にcwdより下のファイルを全部buildする

```bash
$ go install -v ./...
```

`...` が便利

# golang multierror

自分でわざわざ作るよりもテキトウなものを見繕って使ったほうが良い。

```go
package main

import (
	"errors"
	"fmt"

	multierror "github.com/hashicorp/go-multierror"
)

func g() ([]bool, error) {
	var r []bool
	ok0 := true
	ok1 := false
	ok2 := true
	var errs *multierror.Error
	if !ok0 {
		multierror.Append(errs, errors.New("ng0"))
	}
	r = append(r, ok0)
	if !ok1 {
		multierror.Append(errs, errors.New("ng1"))
	}
	r = append(r, ok1)
	if !ok2 {
		multierror.Append(errs, errors.New("ng2"))
	}
	r = append(r, ok2)
	return r, errs.ErrorOrNil()
}

func main() {
	r, err := g()
	fmt.Println(r, err)
    // [true false true] <nil>
}
```

# makefile shellのoutputの結果を使いたいけれど不要なタスクの部分も実行されてしまう事は避けたい


大丈夫だった。これでdefaultではanotherの中の `$(shell make two)` は実行されない

```make
default:
	echo $(shell make one) times

another:
	echo $(shell make two) times

one:
	@echo one

two:
	@echo two
```

[shell functionのヘルプ](https://www.gnu.org/software/make/manual/html_node/Shell-Function.html)見ても大丈夫そうな感じ。
[wildcard function](https://www.gnu.org/software/make/manual/html_node/Wildcard-Function.html#Wildcard-Function) とか知らなかった。
