# python namedtupleでattributeをupdateしたくなった時

```
pip install namedlist
```

recordtypeはpython3.xをサポートしていない。


# golang memo sandbox用のコードで別パッケージのものを書く方法

実際のコードではあまりやるのは推奨されていないが `import .xxx` などと書くと現在のmainからの相対的なimportが記述できる。
例えば以下の様な感じ。

```bash
$ tree ex*
example_private_type
├── main.go
└── ymd
    ├── convert.go
    └── show.go

1 directory, 3 files
```

ここで `main.go` の中で `import .ymd` とすることで、GOPATHに含まれないpackageをimport出来る。


# golang 制限をかけたprivate 型を定義

値を制限する型とかを作れないかどうか考えたりしていた。一番無難なのは以下の様な形かもしれない。

- privateな型を定義
- 公開するのはinterfaceにする

例えば以下の様な感じ。

```go
package ymd

// Month is restricted int for month's expression(1 <= x <= 12)
type Month interface {
	Month() int
}

type month int // private type

func (m month) Month() int {
	return int(m)
}

// NewMonth is factory of Month
func NewMonth(m int) (Month, error) {
	if m < 1 || 12 < m {
		return nil, fmt.Errorf("out of range must be 1 <= x <= 12")
	}
    return month(m), nil
}
```

このようにすると、とりあえず、特殊な迂回したメソッドを実装しないかぎり、制限した範囲の値以外が渡ることを防げるようにはなる。

```go
package main
import ./ymd

// intはymd.Monthではないので明示的な型変換が必要。
func doSomething(m ymd.Month) {
}

func main(){
    m, err := ymd.NewMonth(13)  // error. out of range
}
```


# wip golang idiomatic go toricks

これ良い。

- [Idiomatic Go Tricks](http://go-talks.appspot.com/github.com/matryer/present/idiomatic-go-tricks/main.slide#1)

# wip golang goroutineの使い方

一連の記事読むと良い。

- http://mattn.kaoriya.net/software/lang/go/20160706165757.htm
- http://blog.kaneshin.co/entry/2016/08/18/190435
- http://okzk.hatenablog.com/entry/2016/08/19/121652
- http://okzk.hatenablog.com/entry/2016/08/22/002909


# wip golang 知らないもの

- https://golang.org/pkg/regexp/#Regexp.FindAllStringSubmatchIndex

