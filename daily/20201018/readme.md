## go asttmpl

- 何が嬉しかったのだろう？

  - text/templateから逃れられる点
  - 普通にコンパイルの通ったものからスタートできる

- 何が不足しているんだろう？

  - 型と値しか書き換えられない。
  - 任意の場所にコードを埋め込める -> コメントを利用するhackでできる。
  - まだ構造が変わるようなものなどは埋め込めない？

    - 全体の構造のようなものは直接書き、特定の部分だけブロックとして別に書くことは可能？
    - そもそもtext/template上でtemplateを実行することは可能？

      - できるし、何ならtemplate上で新しいtemplateを定義することも可能

     - 可能なら、関数呼び出しのような見た目に擬態させたい。

      - 関数の中などならできるかもしれない。
      - structのフィールドをループで作るとかが無理？

## go cli

requiredってどんな感じで実装するんだろう？

```go
	seen := make(map[string]bool)
	flag.Visit(func(f *flag.Flag) { seen[f.Name] = true })
	for _, req := range __REQUIRED__ {
		if !seen[req] {
			log.Printf("missing required -%s argument/flag\n", req)
			os.Exit(2)
		}
	}
```

こんな感じか。

### 追記

goのastでいい感じにやるやつ。

- commentに混ぜる
- `__XXX__`に割り当てる

clientのときにどうなるんだろう？引数ごとに使うものを返る。
サブコマンドごとに引数が別か。flagSetを使うのだった。

最後の実行はJSON?

## go text/template

そういえば、goのtext/templateには不慣れなのだよなー。

- range
- define
- template

このあたりを上手く使うといろいろできる？ https://golang.org/pkg/text/template/

あと以下もあるのか

- block
- with

## bash completion

- https://iridakos.com/programming/2018/03/01/bash-programmable-completion-tutorial
- https://blog.cybozu.io/entry/2016/09/26/080000
- https://nujawak.online/blog/170/

### 追記

staticなもの

```
foo(){}
```

に対して

```
_foo_completion(){}
```

を作り

```
complete -F _foo_completion foo
```

という感じで読み込む。ただ補完をするのは別の作業っぽい。compgen。

COMP_WORDS, COMP_CWORD

辞めたくなったら `complete -r foo`。 ~/.bash_completion

## go responseが異なる形状

- たぶん結果をwrapする関数があれば良い
- refを使わないようにしてあげる必要がある。
- reflect.StructOfで作ってあげれば良いはず

### 追記

selectorを作って解決

結局interfaceが分かれてしまった

## go asttempl

- `__xxx__` を穴だと思って利用する
- コンパイルが通る
- typeとvarで型と値を分けられる

関数呼び出しのように展開させる事はできないだろうか？
(text/templにfollback?)
