# go ファイル生成をいい感じにやる

- https://github.com/podhmo/apikit/issues/18

もう少し色々整理する。

## やりたいこと

- 保存時のこと

  - 実行中に失敗したときに、破損した出力が残ってほしくない
  - (ただし、デバッグのために、出力を残した状態での実行も行いたい)
  - ファイルに変更がない場合には、mtimeが更新されないで欲しい
  - 可能なら事前に差分が存在するか確認しておきたい。

- ファイル管理のこと

  - 削除された（使われていない）ファイルを検出したい
  - (a -> bとrenameされたとき、出力が1ファイルではない場合にaを消したいが無理)

## 実装方法

- 出力されたファイルのmtimeとhash値をJSONとして出力する
- 出力したJSONの情報と比較してcreate,update,deleteを決める
- テキトーにsha1とsizeで見れば良い？(mtimeは信用しない)
- コピー的なコマンドを試しに作ってみれば良い？

## create,update,delete

- create -> entryがhistoryに存在しない
- update -> entryがhistoryに存在する
- delete -> history中でentry使われていない

## テキトーにhash値を調べる

checksum目的とはいえあんまり厳密である必要もない気がする。
gitはsha1っぽい。sha1で良いのでは？

- https://qiita.com/KEINOS/items/c92268386d265042ea16
- https://engineering.mercari.com/blog/entry/2016-02-08-173000/

## 実装

stringではなくEntryを持つというのが正しいようだ。Nameでチェックしてその後Hashという流れなので。

```go
type Entry interface {
  Name() string
  Hash() []byte
}
```

## 中間報告

どこまでの機能をもたせるか悩ましい。

- 単に分類するだけの機能
- ファイルを指定するといい感じに読み込んで結果を返す機能
- 実際のディレクトリに記録するところまで含めた機能
- 時刻まで含めてメタデータとして保存しておきたい

全部つなげた場合の機能

1. 各ディレクトリ毎に ".apikit.json". (or tmpdir に `./<dir>.json`)
2. 読み込んでチェック
3. 実行 (or dry run)