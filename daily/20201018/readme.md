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
