## graphql

- ariadneをいじってみた
- strawberryはどうだろう？

### misc

過去にdbから何かを取り出そうとしていたみたいだな。

[../20190414/db-examples/readme.md](../20190414/db-examples/readme.md)

### 本当のgraphqlのsampleを動かしてみるのが良いのでは？

## 試してみたいものはなんだろう？

- databases
- 良い感じでloggingへのintegration (monokaki)
- unittest

## oauth

- auth的なものを手軽につかいたいよなー。
- flask-dance的なやつが他からも使えて欲しい

## encode/databases

## sqlacodegen

- https://pypi.org/project/sqlacodegen/

自作しようかと想っていた。

なんか動かないな。

```
  File "VENV/lib/python3.7/site-packages/sqlacodegen/codegen.py", line 104, in _get_adapted_type
    new_coltype = coltype.adapt(supercls)
  File "VENV/lib/python3.7/site-packages/sqlalchemy/dialects/mysql/enumerated.py", line 310, in adapt
    **kw
  File "VENV/lib/python3.7/site-packages/sqlalchemy/util/langhelpers.py", line 1046, in constructor_copy
    return cls(*args, **kw)
TypeError: __init__() got an unexpected keyword argument 'retrieve_as_bitwise'
make: *** [Makefile:8: 02] Error 1
```

### 追記

昔にそういえば似たようなことをやっていたっけ？
- https://gist.github.com/podhmo/a5550af88d21be5af2c2
- https://gist.github.com/search?utf8=%E2%9C%93&q=sqlalchemy+prestring

何が不足しているかと言うと

- autoincrementが取れている
- 長さが取れていない (length)ことがある？ TINYINT
- server_defaultなどがない
- ondeleteなどがない
- indexなどがない
- relationshipが取れていない
- staff_list (sqlacodegenだとtable)の情報が取れない

そうなってくると、現状の話では正規表現


### そういえば

- inflectionとinflectって何が違うんだろう？

## unittest with asyncio

- asyncio.run()などで包むの微妙じゃん？
- sqlite3でlog出すのどうやるんだろう？

```
conn.set_trace_callback(print)
```

## defaultでやってほしい

- loggingを有効に
- ただし出力は灰色に

https://github.com/encode/databases/issues/75

## sqlalchemy

- metadata, create_all, reflect
- ふつうの実行

## dockerで全部殺したい

- http://sagantaf.hatenablog.com/entry/2018/09/09/003826

```console
$ docker ps
$ docker stop $(docker ps -q)
$ docker rm $(docker ps -aq)
$ docker rmi $(docker images -q)
```

## emacs undo-tree のlimitが小さい気がする

https://github.com/syl20bnr/spacemacs/commit/dcb4da02a8758e7cb700d69689670b85024b068d


