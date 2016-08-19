# golang stack traceを適切に出力するエラー処理

- panicは大丈夫
- errorの時には？ -> [pkg/errors](https://github.com/pkg/errors) 便利

書いた: [使い捨てのコードのエラー処理について - podhmo's diary](http://pod.hatenablog.com/entry/2016/08/18/045136)

# golang time.Timeの取り扱い

[time - The Go Programming Language](https://golang.org/pkg/time/)

## 所定のformatの文字列からtime.Timeを取り出す

```go
s := "2016-08-18T20:32:48+09:00"
t := time.Parse(time.RFC3339, s)
```

## time.Timeから所定のformatの文字列を生成

```go
t := time.Now()
t.Format(time.RFC3339) // 2016-08-18T20:32:48+09:00
```

## fmt.Println()の結果のstringからtimeを取り出す方法

意味がわからないlayoutの意味は、 "1月2日午後3時4分5秒2006年"

fmt.Printlnで出力される文字列をparseするには以下の様な感じにする。

```
layout := "2006-01-02 15:04:05.9 -0700 JST"
s := "2016-08-18 19:54:52.105747747 +0900 JST"
t, err := time.Parse(layout, s)
```

## unix timestampの作り方

```
time.Now().UnixNano() // => 1471517692105747747
```

## layoutとして利用できる定数一覧

time packageの中にlayoutとして利用できるような文字列が幾つか定義されている

```go
// time/format.go

const (
	ANSIC       = "Mon Jan _2 15:04:05 2006"
	UnixDate    = "Mon Jan _2 15:04:05 MST 2006"
	RubyDate    = "Mon Jan 02 15:04:05 -0700 2006"
	RFC822      = "02 Jan 06 15:04 MST"
	RFC822Z     = "02 Jan 06 15:04 -0700" // RFC822 with numeric zone
	RFC850      = "Monday, 02-Jan-06 15:04:05 MST"
	RFC1123     = "Mon, 02 Jan 2006 15:04:05 MST"
	RFC1123Z    = "Mon, 02 Jan 2006 15:04:05 -0700" // RFC1123 with numeric zone
	RFC3339     = "2006-01-02T15:04:05Z07:00"
	RFC3339Nano = "2006-01-02T15:04:05.999999999Z07:00"
	Kitchen     = "3:04PM"
	// Handy time stamps.
	Stamp      = "Jan _2 15:04:05"
	StampMilli = "Jan _2 15:04:05.000"
	StampMicro = "Jan _2 15:04:05.000000"
	StampNano  = "Jan _2 15:04:05.000000000"
)
```

# golang tmpfileの作成

ioutilに `TempFile()` と `TempDir()` が存在する。

```go
import io/ioutil

fp, err := ioutil.TempFile(".", "tmp-")
```

:notebook: 空文字を渡すと `/var/folders/b7/2rk7xp2d0hb2r21zbzjwxb_m0000gn/T/` のような(macの場合)良い感じの名前のディレクトリ内に保存できる。
