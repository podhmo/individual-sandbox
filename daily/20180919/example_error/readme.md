# [golang][memo] pkg/errorsのerrorを独自定義のerrorに使ってしまうと微妙という話

何も考えずにgoimportsで補完された結果のコードを残しておいたら、[pkg/errors](https://github.com/pkg/errors)のerrorが独自定義のerrorに使われてしまって微妙だった。具体的に言うと意図しないスタックトレースを表示してしまうことになる。

例えば以下の様なコードのとき。

```go
package main

import (
	"log"

	"github.com/pkg/errors"
)

// ErrNotFound :
var ErrNotFound = errors.New("not found")

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	return ErrNotFound
}
```

実行してみると以下の様になる。

```
2018/09/20 07:26:27 not found
main.init
	VENV/go/sandbox/example_error/00/main.go:10
runtime.main
	/usr/lib/go/src/runtime/proc.go:186
runtime.goexit
	/usr/lib/go/src/runtime/asm_amd64.s:2361
exit status 1
```

おそらく今回で言えば `run()` の位置を表示することを期待していた。それがmain.initという謎の位置(実際は謎ではないのだけれど)が表示されてしまって、傍目から見てどこで発生したエラーかが分からずという感じになる。なんでダメかと言うと、[pkg/errors](https://github.com/pkg/errors)のerrorは生成したタイミングのstack frameを記録しておくという実装なため(内部的には[runtime](https://golang.org/pkg/runtime/#Callers)パッケージの`Callers()`が呼ばれている)。

## runtime.Callers

ちょっとだけ `runtime.Callers()` を直接使ってみて、stack frameの位置を表示してみる。

```go
package main

import (
	"fmt"
	"runtime"
)

func main() {
	f()
}

func f() {
	g()
}

func g() {
	const depth = 32
	var pcs [depth]uintptr
	n := runtime.Callers(1, pcs[:])
	for _, pc := range pcs[0:n] {
		fn := runtime.FuncForPC(uintptr(pc))
		if fn == nil {
			fmt.Println("<unknown>")
			continue
		}
		file, lineno := fn.FileLine(uintptr(pc))
		fmt.Println(file, ":", lineno)
	}
}
```

pythonなどでcurrent frameを取ったりするのとまぁおんなじような感じ。

```
VENV/go/sandbox/example_error/02/main.go : 19
VENV/go/sandbox/example_error/02/main.go : 14
VENV/go/sandbox/example_error/02/main.go : 10
/usr/lib/go/src/runtime/proc.go : 207
/usr/lib/go/src/runtime/asm_amd64.s : 2362
```

ここまで来ると何がダメだったのかは明らか。この`Callers()`が呼ばれた位置を記録するため。実行時（error生成時）の位置が表示されていた。

## おそらくやりたかったこと

標準ライブラリのerrors方のNewのつもりで使っていた。それが誤ってpkg/errorsの方の`New`が呼ばれてしまって。。という感じ。goimportsによるimport文の補完結果をユニークにするなら、fmtの方を使えば良かったかもしれない。こういう感じ。

```go
package main

import (
	"fmt"
	"log"

	"github.com/pkg/errors"
)

// ErrNotFound :
var ErrNotFound = fmt.Errorf("not found")

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	return errors.Wrap(ErrNotFound, "on run")
}
```

今度は期待通り。

```
2018/09/20 07:36:33 not found
on run
main.run
	VENV/go/sandbox/example_error/01/main.go:20
main.main
	VENV/go/sandbox/example_error/01/main.go:14
runtime.main
	/usr/lib/go/src/runtime/proc.go:198
runtime.goexit
	/usr/lib/go/src/runtime/asm_amd64.s:2361
exit status 1
```
