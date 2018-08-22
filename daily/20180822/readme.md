## emacs lisp prevent opening at the side window

https://www.gnu.org/software/emacs/draft/manual/html_node/elisp/Choosing-Window-Options.html#Choosing-Window-Options


```lisp
(setq split-height-threshold nil)
(setq split-width-threshold nil)
```

これだけじゃだめ。これはdisplay-bufferなどのときの操作。

## 手慰みの作業

- graphknife
- matcher,combobox -->
- configknife
- handofcats
- numpy puzle
- 自分の行動範囲の整理
- driver(segmetrics)

## 自分の行動範囲の整理

- dictknife
- kamidana
- jqfpy
- (miniconfig)

- swagger-marshmallow-codegen
- reqtrace
- prestring
- nbreversible
- magicalimport
- goaway

updateしたい

- utatane
- familiar
- nejimaki
- monokaki
- toybox
- reportlogging
- minidsl

@:ASTを管理する何かを。typeをもっと便利に。
@:UI関係どうにかしたい
@:Config(path,data)という表現便利だった
@:python benchmark


## python benchmark library

- https://github.com/vstinner/perf
- https://github.com/cheind/py-motmetrics

### profier

- memoryprofiler
- lineprofiler
- (timeit)
- cprofile
- procutil

## python benchmark suite

- https://github.com/python/performance

## matcher

- re.compileとかをcacheしたい(N個)
- JSONで引き渡したい
- 最終的にCに
- １つ定義を書いたら他の言語でも対応されて欲しい

## configknife

- mergeとかできる感じで（継承？）
- ネストした読み込みができて欲しい
- configというstruct

## dictknife

- transformer(dictremapper)
- validation
- diff

## handofcats

- choices
- typed

## hmm

- 設定ファイルがある
- 設定ファイルを見る
- コード生成がある
- single source of truth
- 生成が早い
- 生成後はすぐに試せる
- 試すときの例が存在する
- エラーが出たらすぐわかる
