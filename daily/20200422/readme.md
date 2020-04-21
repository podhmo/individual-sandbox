## cligen

main.goのことを考える

- コマンドライン引数の解析
- 依存のセットアップ
- runtimeの実行

関数定義とみなすと、引数の解析は明らかになったがbody部分の挙動があやふや。
正確に言うと、挙動があやふやなのではなく定義があやふや。

ここで、実装の詳細に立ち入ってほしくないということでもあるのだけれど。

### いい感じにみたい

### 設定を一元管理したい

例えば、こういう感じ。

```
cmd/
├── definitions.py
├── bye
│   └── main.go
└── hello
    └── main.go
```

ここで、definitions.pyは全部見える。

```python
def hello(*, name: str, nickname: t.Optional[str]=None):
    """call hello()"""
    pass


def bye(*, name: str):
    """call byebye()"""
    pass
```

ネストしたサブディレクトリに置きたい場合とかは？

`__`を`/`として扱っちゃえば。例えば `greeting__hello()` は `greeting/hello` になる。

rootは？適当にrootとかbasePathのような名前でつければ良いのでは？


### 既存の実装を壊さないようにしたい

main.goに直接処理が書いてある事がある。

- さすがにmain()一個だけの場合はどうしようもない
- 助けたいのはヘルパー関数などがその場で定義されているような状況

```
// START: emitted by xxx (DON'T TOUCH)
func main(){
}

func run(opts *options) error{
}

type options {
  ...
}
// END: emitted by xxx (DON'T TOUCH)

func doSomething(x X, y Y) error {
  ...
}
```

### go側の詳細

(実は依存を注入する部分とコマンドライン引数の解析は別々に作れる？)

依存のsetup

```
ob := LoadOb(x)
ob, err := LoadOb(x)
ob, teardown := LoadOb(x)
ob, err, teardown := LoadOb(x)
```

最終的なcont

```
return Hello(ctx, ob)
return batch.Cron{Expr: expr, Ob: ob}.Run(ctx)
return batch.Loop{}.Run(func (ctx context.Context) err { return Do(ctx, ob)})
```

goの型をpythonの型として

- primitive
- composite
- alias

aliasはつけられるということにするとよいのかも(shared?)。

## component factory (provider)

- 全部のproviderを定義するパッケージとか作りたくない
- 全部依存は埋め込みにしてしまうというのは手かも
- main.goを小さくしたい (refのようなものを用意する)

あと、スタブを作る？

### stubs

- DB
- DB2 (dbとは別の所)
- logger
- (dispatcher)
- mailer
- slack notificator
- SQS
- Elasticsearch
- APIClient
- APIServer あるいはコマンド 本体

### misc

- 実行時にだけ有効なオプションもあるかもしれない (e.g. dry run)
- 細々としたpatchを入れたくなるかもしれない (e.g. httptrace)

## python

ほしいのはConfiguratorでは？

## go flagの挙動の確認

丁寧に調べる

## go python 生成

cligen的なあたりを目指したい

- 通常の実行では単に何が読み込まれたかを表示するだけにしたい
- 特定のコマンドを利用して実行した場合にはコード生成として機能
