# goのtesting.Mを使った関数がpackage単位のfixtureとして使えるか調べてみる

goのtesting.Mを使った関数がpackage単位のfixtureとして使えるか調べてみる。例えばdbのリソースの取得・破棄などの処理を少なくともpackage単位で行うようにしたい（単にtesting.Tをとるテスト関数のdeferでやってしまった場合には関数毎にリソースの取得・破棄が行われてしまう）。

## testing.M ?

testing.Mが使えそう。

https://golang.org/pkg/testing/#hdr-Main

> It is sometimes necessary for a test program to do extra setup or teardown before or after testing. It is also sometimes necessary for a test to control which code runs on the main thread. To support these and other cases, if a test file contains a function

```go
func TestMain(m *testing.M)
```

そして記憶が確かなら、gotestはpackage単位でbuildして実行を繰り返す。みたいな形になっていたはず。なのでMainと言いつつpackage単位でのリソースの破棄などに使えそうな感じ。

## 実験

実際に実験してみる。以下の様な雑な構造のパッケージを用意する。foo,foo/bar,foo/booのパッケージのそれぞれでtesting.Mを利用した関数の出力を見てみる。


```console
$ tree
.
├── bar
│   ├── bar0_test.go
│   ├── bar1_test.go
│   └── main_test.go
├── boo
│   ├── boo0_test.go
│   ├── boo1_test.go
│   └── main_test.go
├── foo0_test.go
├── foo1_test.go
├── main_test.go
└── readme.md

2 directories, 10 files
```


それぞれ以下の様な関数を定義してみた。(下の例はfooのもののみ)

foo/foo0_test.go

```go
package foo

import "testing"

func TestFoo0(t *testing.T) {
    t.Log("0")
}
```

foo/main_test.go

```go
package foo

import (
	"fmt"
	"reflect"
	"testing"
)

func TestMain(m *testing.M) {
	m.Run()
	fmt.Printf("end tests=%#+v\n", reflect.ValueOf(m).Elem().FieldByName("tests"))
}
```

以下の様な形でログが出力されるようなら大丈夫そう。

```
setup main
  test 0
  test 1
teardown main
```

## 実験結果

実際に試した結果。良い感じ。

```console
$ go test -v ./...
=== RUN   TestFoo0
--- PASS: TestFoo0 (0.00s)
	foo0_test.go:6: 0
=== RUN   TestFoo1
--- PASS: TestFoo1 (0.00s)
	foo1_test.go:6: 1
PASS
end tests=[]testing.InternalTest{testing.InternalTest{Name:"TestFoo0", F:(func(*testing.T))(0x4e71c0)}, testing.InternalTest{Name:"TestFoo1", F:(func(*testing.T))(0x4e7240)}}
ok  	./foo	0.002s
=== RUN   TestBar0
--- PASS: TestBar0 (0.00s)
	bar0_test.go:6: 0
=== RUN   TestBar1
--- PASS: TestBar1 (0.00s)
	bar1_test.go:6: 1
PASS
end tests=[]testing.InternalTest{testing.InternalTest{Name:"TestBar0", F:(func(*testing.T))(0x4e71c0)}, testing.InternalTest{Name:"TestBar1", F:(func(*testing.T))(0x4e7240)}}
ok  	./foo/bar	0.001s
=== RUN   TestBoo0
--- PASS: TestBoo0 (0.00s)
	boo0_test.go:6: 0
=== RUN   TestBoo1
--- PASS: TestBoo1 (0.00s)
	boo1_test.go:6: 1
PASS
end tests=[]testing.InternalTest{testing.InternalTest{Name:"TestBoo0", F:(func(*testing.T))(0x4e71c0)}, testing.InternalTest{Name:"TestBoo1", F:(func(*testing.T))(0x4e7240)}}
ok  	./foo/boo	0.001s
```

