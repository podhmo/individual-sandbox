## python invoke

- https://www.pyinvoke.org/

```console
$ pip install invoke
```

基本的には以下

```console
# 一覧
$ invoke --list

# help
$ invoke --help <task>

# run
$ invoke <task>
```

コードの書き方は

```python
import invoke

@invoke.task
def <task>(c):
    c.run("echo hello")
```

## python arch python3.8

```
$ yay -Syuu
```

python3.8になった

- ~/.emacs.d/.python-environment
- ~/venv/my

めんどくさかったのでlib/python3.7をlib/python3.8にrenameした


## metashape python Context部分の属性を良い感じにしたい

scan後の操作を色々覗いてみると良いかもしれない

- https://github.com/goadesign/goa
- https://github.com/OpenAPITools/openapi-generator
- https://github.com/go-swagger/go-swagger

## openapi-generator

次はopenapi-generator

openapi-generatorは素直にmustacheっぽいな。
parseした結果をどうするんだろう？



## goaのコードの処理を見てみる

ディレクトリ構造はこんな感じ。記憶が確かならgoaはtemporaryにmain.goを出力してソレを実行という感じだった記憶。

```console
$ tree -d -I "*test*" --ignore-case
.
├── cmd
│   └── goa
├── codegen
│   ├── cli
│   ├── example
│   ├── generator
│   └── service
├── docs
├── dsl
│   └── _spec
├── eval
├── expr
├── grpc
│   ├── codegen
│   ├── docs
│   ├── middleware
│   │   └── xray
│   └── pb
├── http
│   ├── codegen
│   │   └── openapi
│   └── middleware
│       └── xray
├── middleware
│   └── xray
├── pkg
└── security

27 directories

```

まぁそのへんはけっこうどうでも良くてDSLとの関係が分かれば良い。

### plugins

まぁpluginの機能を読んでいけば良さそう。

https://goa.design/extend/plugins/

以下の様なメソッドを持ったものらしい

- GenerateFunc()
- PrepareFunc()

この組み込みのCORSのpluginが参考に成るとのこと。

https://github.com/goadesign/plugins/tree/v3/cors

### もうすこしpluginを

DSLが定義されている。

https://github.com/goadesign/plugins/blob/v3/cors/dsl/cors.go

- Origin()
- Methods()
- Expose()
- Headers()
- MaxAge()
- Credentials()

基本的には内部の`eval.Current()` とかを呼び出す。これは

- goa.design/goa/v3/eval

なるほどeval側に状態を飼っちゃうのか。

あー、そうだったそうだった。こういう感じだった。

```go
var _ = dsl.XXX(...)
```


### 気になるオブジェクト

概ねやっているのはtemplateの中なのか

- codegen.File
- codegen.RegisterPlugin()
- codegen.SectionTemplate

基本的には以下のような関数らしいな。

https://github.com/goadesign/plugins/blob/369466b9ecc3a45bd8e60613f28b897737d81fd2/cors/generate.go#L40

```go
// Generate produces server code that handle preflight requests and updates
// the HTTP responses with the appropriate CORS headers.
func Generate(genpkg string, roots []eval.Root, files []*codegen.File) ([]*codegen.File, error) {
	for _, f := range files {
		serverCORS(f)
	}
	return files, nil
}
```
