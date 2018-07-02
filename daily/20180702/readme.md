## python しりとりというのは手軽な入門用に良い？

https://github.com/wagase/pokeshiri

気になる点

- requirements.txtだけでもほしい
- setup.pyがない(packageじゃない)
- global変数(保存・再起動ができない)
- テストがない
- pep8じゃない(yapfが掛かっていない)
- 設定ファイルがpython(.jsonか.envが良い？,pythonでも良いけれど)
- しりとりのデータはポケモンにこだわらなくできるのでは？
- slackに繋げないと確認できない
- loggingの設定がmainにない

pluginsで提供されているのか

細々

- dictのinの使い方
- moduleを読み込むタイミングで定数(JSON)を定義(from 外部ファイル)
- routing使っていないのもったいない
- append使わずindexアクセスは辛い
- ンを除外するものと除外しないものを取るやつが辛い(２つ似たような形)
- formatかf-stringを使ってほしい
- 結果を表すメッセージ(文言)とstatusが一緒になってる
- ィ -> イ とか手軽にできない？
- 関数にするかクラスにするか
- モジュールの分け方

自分なら

- asyncioベースで作る?(websockets)
- slackのbot部分から？
- port, handlers, store
