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
- subscription
- pluginシステムをもうちょっと良い感じにしたい?


機能拡張

- 他のdataでもいけるように
- 再開できる
- 結果を記録できる
- 絵とか表示したい

### hmm

```
myfunc.getpokenamelist()
mention_func()
  # reset
    - myfunc.reset()
      - myfunc.makekanalistGetnn()
      - myfunc.makekanalistNotnn()
  # log
  # ranking
    - myfunc.remarkRanking()
  # hint
    - myfunc.hint(req)
  # 詳細
    - myfunc.getpokedetail(req)
    - myfunc.checkExistenceAllPoke(req)
  # else
    - myfunc.checkExistencePoke(req)
    - myfunc.memoryRemark(req)
    - myfunc.checkExistencereq(req)
    - myfunc.countreqstock(req)
    - myfunc.checkTruelastword(req)
    - myfunc.forgivelastword(req)
    - myfunc.shiritori(req)
      - myfunc.getshiri(req)
      - myfunc.delstock(kana, val)
      - myfunc.pokechoice(kana)
        - myfunc.delnstock(kana, val)
        - myfunc.memorylastword(req)
        - myfunc.reqstockappend(req)
```

helpers

```
- myfunc.left(text, e)
- myfunc.mid(text, s, e)
- myfunc.right(text, s)
```

