# metashapeの実験

とりあえず、pythonのクラス定義からdictを作ってみるか。

- トップレベルの定義を決めるのがむずかしいなー
- conflictチェックとかあったほうが良い？
- ネストしたようなクラスってどうなるんだっけ？

## 00

とりあえずかんたんなクラスを。
何か悪くはないんだけれど、設定ファイルっぽくないかも。
次はもう少しネストした表現などを含めて考えてみるか。

snakecaseなどへの変換とかどうすると良いんだろうなー(したくなる？)。
(type walkerのiteratorが返す値が長さが3のtupleなのはけっこう辛いかもしれない？)

## 01 nested

テキトーにmkdocs辺りの設定ファイルを参考に作ってみるか。
（本当はschemaと設定値を気にしたくなる(わかる)）

色々つらいことが分かった。

- 予約語の対応がだるい
- ネストしたクラスだとクラス定義とフィールドへの代入が必要になる
- リストへの対応がだるいかも

### hmm

まだもうちょっとダメ

- magicalimport.import_symbol で type.__module__ が None
- importlib.import_moduleもなんかおかしい
- ふつうのimportでもおかしい

### 追記

原因が分かった。１つはmagicalimport経由だとexec_moduleの前にsys.modulesに追加していない。
もう１つはdataclassのobjectが渡っていてTypeErrorになっていた。TypeErrorの時に無視されていた。
