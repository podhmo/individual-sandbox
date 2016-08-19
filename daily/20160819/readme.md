# golang ファイルの読み書きが存在する処理のテスト

たかだかunit testのために実ファイルを作るのはバカバカしい。
pythonで言う StringIO のような物がほしい。

方針としては以下の様にする。

- 公開関数としてのFoo(string -> IO)を作成
- 非公開関数としてのfoo(io.Reader -> IO)を作成

```go
func Foo(filename string) error {
	if _, err := os.Stat(filename); err != nil {
    		return err
	}
	fp, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer fp.Close()
	foo(fp, os.Stdout)
	return nil
}

func foo(r io.Reader, w io.Writer) error {
}
```

テストの時にはfooだけテストする。

## stringIO的な何かは `bytes.Buffer`

StringIO的な振る舞いをするものがほしい時には、[bytes.Buffer](https://golang.org/pkg/bytes/#Buffer)を使えば良さそう。

byteではなくstringから生成したい場合には `NewBufferString()` を使えば良い

```go
var r io.Reader
r = bytes.NewBufferString("<text message>")
```

## `fmt.Printf()` 的なものの呼び出しがある場合には？

`fmt.Printf()` 的なものの呼び出しがある場合には、`fmt.FPrintf()` に `os.Stdout`　渡す感じにするのが無難かもしれない。


```go
// これを
fmt.Printf("hai")

// こうする
fmt.Fprintf(os.Stdout, "hai")

// 実際には
var w io.Writer
w = <>
fmt.Fprintf(w, "hai")
```
