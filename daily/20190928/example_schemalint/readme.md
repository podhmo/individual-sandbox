# schemalint

- https://github.com/podhmo/schemalint

えーと、とりあえずdocker-compose.ymlあたりが補完できるようになると嬉しい。
まぁ補完以前にlintだけでも。

## activate

- [../../20190921/example_scemalint/setup.el](../../20190921/example_scemalint/setup.el)をわすれずに。

## docker-compose.yml

- schema用のrouterが欲しい
- 幾つかのenumが存在していて完全指定
- latest的なversionが必要になる
- `3` -> `3.7` みたいな変換が必要になることも

## 追記

そういえば、soft errorとhard error的な差も欲しい？
INFO的な出力のときには成功してほしいし。

## 追記

とりあえずで動かせるようにしてみている。

- cacheが欲しい？
- validation errorの表示位置がおかしい？
- raw outputを表示したい 
- raw outputをflycheck compileで表示できる？
- 例を表示したい

## validationをきれいな感じに

- requiedエラー
  - 親（ぜんたいにlint貼りたい？）

## コードをキレイに

- $schemaの対応のためにdocを見るようにしたい
- lintのoutputをわかりやすく
- visualizeオプション的なものを追加する？


