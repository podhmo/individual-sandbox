## 作業

- flutterのwidget全体を眺める
- ok graphqlと親しむ
- gqlgenのtodoのコードを把握する
- gqlgen+dataloaderまでのコードを見る
- graphqlのparser周りと親しむ
- yet anotehr language server

## emacs use-package python

pythonの設定が正しい感じがしないかも？

- with-eval-afterl-loadのミス？
- use-packageのミス？

後者の感じっぽい。こんな定義になっていたとしてffap-pythonが呼ばれないとquickrunの設定が実行されないみたい。

```
  (use-package ffap-python  ;; mine
    :commands (ffap-python ffap-python:find-program ffap-python:find-python)
    :config
    (setq ffap-python-disable-confirm-before-open t)
    ...
    )
```

### use-packageのこと

- http://grugrut.hatenablog.jp/entry/2019/01/05/204044

```lisp
(setq use-package-compute-statistics t)
```

M-x use-package-report

```
Show current statistics gathered about use-package declarations.
In the table that’s generated, the status field has the following
meaning:
  Configured        :config has been processed (the package is loaded!)
  Initialized       :init has been processed (load status unknown)
  Prefaced          :preface has been processed
  Declared          the use-package declaration was seen
```

実際 Initialized だった。ffap-pythonを実行するまで。

- https://qiita.com/kai2nenobu/items/5dfae3767514584f5220

本文は古いけれど役に立つ話。

## python graphql-core-next parserとかどこでもっているんだろう？

- https://github.com/graphql-python/graphql-core-next
- https://github.com/michaelaquilina/flake8-graphql
- https://github.com/mirumee/ariadne

知りたいのはgraphqlの構文の parser?

### parserはどこ？

- https://github.com/graphql-python/graphql-core-next/blob/7649646cb1530a9dfdb76e53ec2313830ca94899/src/graphql/language/parser.py

- docs https://graphql-core-next.readthedocs.io/en/latest/

## graphql grammer

- https://github.com/graphql/graphql-spec
- https://github.com/graphql/graphql-spec/blob/master/spec/Appendix%20B%20--%20Grammar%20Summary.md
- https://github.com/antlr/grammars-v4/blob/master/graphql/GraphQL.g4
- https://graphql.org/graphql-js/language/

## graphql なんかまとめておきたいことなんかあったっけ？

- https://graphql.org/learn/best-practices/

## graphql + httpie

http://jonasbn.github.io/til/httpie/using_httpie_with_graphql_server.html

こんな感じで追加できる？

```console
$ echo '{"query": "mutation { createTodo(todo: {text: \"hello\", done: false}){id} }" }' | http --json POST :8081/query

$ http --json POST :8081/query query='mutation { createTodo(todo: {text: "hello", done: false}){id} }'
```

## httpie の指定部分のcheat sheet

https://httpie.org/doc#request-items

## go gqlgen + dataloaderを動かしてみる

- とりあえずgqlgenを動かしてみる
- dataloaderのexampleを動かしてみる
- sqlx含めて何か良い感じに使う

という感じのことをやりたいかも。
コード生成系を直接ここで使うのあんまり良くなかった。

### 追記

examples

- https://github.com/99designs/gqlgen/tree/master/example

```console
$ cd $GOPATH/src
$ mkdir -p github.com/99designs && github.com/99designs
$ git clone git@github.com:99designs/gqlgen
```

(find-file "~/go/src/github.com/99designs/gqlgen/example/readme.md")
(browse-url "http://localhost:8081")

## 追記

graphqlの感覚

- "#/schema/query" に対応するqueryから実行できる
- typeは詳細を書く必要がある？（全体を取るとかできないのだっけ？)

graphqlを思い出すところだけで終わってしまった。

## 昨日の作業の続き

- flutterの中を覗く
- gqlgen + dataloader
- sqlxのまとめ
- ctxlog
