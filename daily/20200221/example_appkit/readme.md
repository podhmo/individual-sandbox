# appkit

簡単に言えばgoのmain.goを生成する君

- CLIのoptionを良い感じにやる
- componentsのDIを良い感じにやる

特に今回は後者。

## 気になること

- 型の扱い
- pointerの扱い
- context?
- 変数名の管理がめちゃくちゃだるい

### 追記

一旦全部リセットして考えると、名前の管理がめんどくさい。

- import foo
- fooOb := foo.Foo()

そしてgoのそれそのものというのはけっこう使いづらいのでは？という感覚を持った。

## simplify codeobject

- Assignはいらないのではないか？

  - letのタイミングで文を出力
  - 後々はsymbolとして機能

- Goのimportに対応できない

  - Module継承してしまっているのが邪魔
  - Goの方も書き換えたい(importがない)
  - 混乱を招くしModuleを継承するのは悪手では？

- prestringでfuncにwithをやって取れるのは関数名のsymbolでは？
- prestringでstmtの挙動を追加してもよいはず
- prestringでimportのときにsymbolを返すべきでは？

## python import

- import foo
- import foo as bar
- from foo import bar
- from foo import bar as baz
