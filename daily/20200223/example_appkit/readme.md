# appkit

[../../20200221/example_appkit/readme.md](../../20200221/example_appkit/readme.md)の続き

## その前にprestringを書き換えた

- mypy ready
- async対応
- codeobjeobjectを追加した
- importでsymbolを返すようにした

### codeobject

- ok Assignはいらないのではないか？

  - letのタイミングで文を出力
  - 後々はsymbolとして機能

## 今回試すこと

- Goのimportに対応できない

  - goのimportどうしよう？ import_groupは消し去りたい
  - ついでにpackage名などもどうしよう
  - goではファイルという概念が増える

- pointerをどうしよう

## hmm

componentをどう定義するかという気がしてきた。
import時のmoduleを使うようにしたい。

