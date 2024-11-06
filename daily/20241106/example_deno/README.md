# 関数に型チェックを追加した変形を返したい

>[!WARNING]
>まだ未解決

Pythonのargparseのchoicesを実装したい。
機能をTypeScriptの言葉で説明するとある特定のフラグに対応するフィールドの型がある `literal union` であることを保証するもの

## choices0.ts

とりあえず、愚直に実装してみる。
直接 `@std/cli` のparseArgsを使い直接追加のフィールドを返す。

- 📝 ついでに `literal union` の型と一緒に実行時のチェックのための値も定義する方法を調べておく
- 📝 関数自体の型はtypeofとParameters,ReturnTypeで取り出せる

一応動いているようだ

## choices1.ts

そういう関数を返す関数を返したい。最も基底の関数の型はなんだったっけ？ mapped typeで名前をつけられたっけ？
一つ関数を実装するたびに毎回フルの型を書くのは辛すぎるし上手く伝搬しているかもわからない。

理想を言えば `parseArgs = AddChoices(parseArgs, "direction", directions)` みたいなことをしたい

## choices2.ts

もっと愚直に直書きしてみる。
ヘルプメッセージがほしい。これはヘルプメッセージを作る関数をimportして呼べれば良いだけの話かもしれない。

ちなみにこちらの実装を選択したのはラップした関数の型を整えるのが面倒だったから。理想を言えばchoices0.tsの実装をしたい。

## choices3.ts

choices0.tsの方法とchoices2.tsの方法を組み合わせようと頑張ってみた。

と思って実装してみたもののやっぱりneverになってしまう。これは型引数を引き回せてなかったからなのかな？。

どうやら `@std/cli` でできていることが `@podhmo/with-help` ではできていない。

## choices4.ts

以下のような関数があれば十分なのでは？とおもったけれどだるいな。。

```ts
let parsed = parseArgs(Deno.args, {...} as const)

// Parsed<> & {direction: DirectionType} が返ってくる
// 失敗したらヘルプメッセージを出してDeno.exit()
parsed = requireChoices<DirectionType>(parsed, "direction", directions)
```

しかしこの方法だとconstで記述しきれなくてだるいかも？

### 続き


こんな形で推論できないかと思ったがなぜかstringになってしまうな(widing)。

```ts
requireChoices(parsed, { direction: directions } )
```

これらの型は全部違う？
DirectionTypeはtypeofを呼んでいるから値から型に代わっているのか。

```ts
const directions = ["north", "south", "east", "west"] as const;
type DirectionType = typeof directions[number];
type K = readonly ["north", "south", "east", "west"];
const _v : K = directions

// DirectionType   is "north" | "south" | "east" | "west"
// DirectionType[] is ("north" | "south" | "east" | "west")[]
// K               is readonly ["north", "south", "east", "west"]
```