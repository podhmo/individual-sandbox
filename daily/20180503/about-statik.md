#[golang][statik] go-bindataの代わりにstatikを使うことにした

以前はgo-bindataを使っていたのだけれど。そういえば、作者がアカウント削除したあとに他の人がリポジトリ作成したみたいなことがあったな−。ということを思い出したりしたので代替品を探してみることにした。

statikを使う。statikはこちら。

- https://github.com/rakyll/statik

正直な所どれでも良いのだけれど。必要なパッケージのインストールが以下だけで済むのが気に入った。

```
go get -v github.com/rakyll/statik
```

使いかた自体は、`statik`コマンドが使えるようになるのでREADMEの通りに使えば良い。

例えばconfig以下を埋め込むなら以下のようにする(statikというディレクトリが生成される)。

```
$ statik -src config -f
```

## http.FileSystem

`http.FileSystem`のインターフェイスを実装したものを通じてアクセスする。これは以下のようなインターフェイス。

```go
// A FileSystem implements access to a collection of named files.
// The elements in a file path are separated by slash ('/', U+002F)
// characters, regardless of host operating system convention.
type FileSystem interface {
	Open(name string) (File, error)
}

// A File is returned by a FileSystem's Open method and can be
// served by the FileServer implementation.
//
// The methods should behave the same as those on an *os.File.
type File interface {
	io.Closer
	io.Reader
	io.Seeker
	Readdir(count int) ([]os.FileInfo, error)
	Stat() (os.FileInfo, error)
}
```

すっごく薄いvfs(virtual file system)と考えてみても良いかもしれない。

```go
import "github.com/rakyll/statik/fs"
import __ "./statik" // まじめにやるならabsolute path

FS, _ := fs.New()
f, _ := FS.Open("/message.txt") // 先程-srcで指定したconfigディレクトリのmessage.txtが参照できる
defer f.Close()
io.Copy(os.Stdout, f)
```

ところで、個人的には `http.FileSystem` のようなインターフェイスは苦手で、これをmapと似たようなものと捉えるなら、どのようなkeyでアクセスできるかの一覧が欲しい。別な表現をするなら、ディレクトリに対する`ls`ないしは`find`のような操作が行いたい。

`fs.New()`の返す値は、内部的には以下の様な定義だった。

```go
type statikFS struct {
	files map[string]file
}
```

unexportedなfieldに値が格納されているので、reflectを使って無理やり取り出す。以下の様な感じにすれば良さそう。
(ファイルのパスだけ知りたいのならkだけで十分。中のbytesを取り出したいならMapIndexなどを使って取り出している処理が必要になる)

```go
import "github.com/rakyll/statik/fs"
import __ "./statik" // まじめにやるならabsolute path

FS, _ := fs.New()

rm := reflect.ValueOf(FS).Elem().FieldByName("files")
for _, k := range rm.MapKeys() {
	v := string(rm.MapIndex(k).FieldByName("data").Bytes())
	fmt.Println(k, v)
}

// /message.txt hello
```

まぁ、そんなわけで、`config/message.txt` は `"/message.txt"` でアクセスできる。

## 追記

似たような記事が既にあった。こちらはgo-assetsだけれど。

- [go-bindataを使っていた部分をgo-assetsに置き換える - PartyIX](https://h3poteto.hatenablog.com/entry/2018/02/25/215810 "go-bindataを使っていた部分をgo-assetsに置き換える - PartyIX")

（ところで参照先の記事はなんで `json.Decoder` などを使わず、`json.Unmarshal()` を `bytes.Buffer` 越しに使おうとしているのだろう？）
