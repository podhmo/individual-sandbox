# gitで不要なファイルを永久追放

たまにしか使わないので忘れる

```bash
$ git filter-branch --tree-filter "rm -f <filename>" HEAD
$ git gc --aggressive --prune=now
```


# golang go testの際に標準出力を殺さない方法

`-v` 付ければ良い `go test -v`


# golang ファイルの読み書きが存在する処理のテスト

以下の２つの場合が考えられる。

- tmpfileを直接作成する場合
- stringIO的なものを利用する場合(ファイルは作成しない)

## tmpfileを直接作成する場合

tmpfileの作成にはio/ioutilの関数を使えば良いという話だった。

例えば、以下のような `prepareFile()` を作ってどうにかする？
(並行したアクセスがあることは考えていない)

```go
func prepareFile(filename string, content string) (*os.File, error) {
	// setup
	fp, err := ioutil.TempFile(".", filename)
    defer fp.Close()
	if err != nil {
		return nil, err
	}

	w := bufio.NewWriter(fp)
	w.WriteString(content)
	w.Flush()
	return fp, nil
}

func TestFoo(t *testing.T) {
	// setup
	fp, err := prepareFile("testtext-", "hello")
	if err != nil {
		t.Error(err)
	}
	tmpName := fp.Name()

	// teardown
	defer os.Remove(tmpName)

	// test main
    // do something
}
```

## stringIO的なものを利用する場合(ファイルは作成しない)

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

### stringIO的な何かは `bytes.Buffer`

StringIO的な振る舞いをするものがほしい時には、[bytes.Buffer](https://golang.org/pkg/bytes/#Buffer)を使えば良さそう。

byteではなくstringから生成したい場合には `NewBufferString()` を使えば良い

```go
var r io.Reader
r = bytes.NewBufferString("<text message>")
```

### `fmt.Printf()` 的なものの呼び出しがある場合には？

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
