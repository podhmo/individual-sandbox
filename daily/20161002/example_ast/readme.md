## 00 ast

setup

```
$ pip install codegen
```

問題点

- commentが消える
- python3用のコードを追加すると関数の引数定義が上手く出力されない(monekey patchや対応が必要)


## 01 baron

setup

```
$ pip install baron redbaron
```

問題点

- リダイレクトするとValueErrorが出る(log出力のあたりでの文字列生成のあたりがおかしい？)
- リダイレクトしないと正常に終了するのが謎
- 先頭のコメント後に空白というコードがコメントの重複という結果になる。
