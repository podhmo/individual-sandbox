# golang method により struct -> interfaceの解釈が入るとnilではなくなる。

以下の様な定義がある。

```go
type barker interface {
	Bark()
}

type dog struct {
}

func (d *dog) Bark() {
	fmt.Println("\tわんわん")
}

func play(b barker) {
	fmt.Printf("\tplay with %[1]T: %[1]v\n", b, b)
	if b != nil {
		b.Bark()
	}
}
```

ここで、dog型のzero valueをreceiverとして `play()` を呼び出すと、 `b != nil` の部分がtrueになってしまう。

```
var d *dog // この時点ではnil
play(d)

/*
	play with *main.dog: <nil>
	わんわん
*/
```

# 妖しいもの

gistにuploadしたものをpackageとして利用できるみたいな感じ？(古いのでだめかも)

- [ImJasonH/go-gist: Go import redirector for GitHub Gists](https://github.com/ImJasonH/go-gist)

# golang コマンド的なものの作り方

以下の２つの方法がありそう。(pecoなどは後者)

- すごくすごく単純なもの
- それなりに階層が複雑なもの

## すごくすごく単純なもの

repository名と同一のファイルを１個作る。readme.mdもあっても良い。

例えば、 `https://github.com/<username>/foo` に `foo.go` を置いたrepositoryを作る。
すると以下で `$GOPATH/bin` に入る。

```
go get github.com/<username>/foo
```

例えば[こういう感じ](https://github.com/podhmo/individual-sandbox/tree/master/daily/20160820/hai/)で作れる


## それなりに階層が複雑なもの

だいたい以下の様な構成になっていることが多い。今回はfooというpackage(コマンド名でもある)を作る場合の例。

```
foo
├── cmd
│   └── foo
│       └── main.go
└── something.go
```

これで `go get github/<username>/foo/cmd/foo` でインストールできるようになる。
またpackageの書き方は概ね以下の様な感じ。

トップレベルのfoo(e.g. something.go)内

```go
package foo

import (...)
```

コマンド部分の階層(e.g. cmd/foo/main.go)

```go
package main

import (
    github.com/<username>/foo
)

// foo.XXX みたいな形で使う。
```

# golang 統一的なマスキングの処理をしたい場合どうすれば良いんだろう？

reflect package使わないとダメなのかな？

```go
type User struct {
	Name     string
	Password string
}

type Maskable interface {
	Mask() interface{}
}

func Mask(o interface{}) interface{} {
	v := reflect.ValueOf(o)
	if t, ok := v.Interface().(Maskable); ok {
		return t.Mask()
	}
	return o
}

func (u User) Mask() interface{} {
	// object u is copied.
	rx := regexp.MustCompile(".")
	u.Password = rx.ReplaceAllLiteralString(u.Password, "*")
	return u
}
```

いや、reflect packageは不要。 type assertionがあればどうにかなりそう。

```go
type I interface {
}

func Mask(o I) I {
	if o, ok := o.(Maskable); ok {
		return o.Mask()
	}
	return o
}
```

これでも良い。

```go
func Mask(o I) I {
	switch o := o.(type) {
	case Maskable:
		return o.Mask()
	default:
		return o
	}
}
```
