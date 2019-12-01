# 実はhandofcatsを裏側に使ってsubcommandの対応も手軽にできるのでは？

できそう。ただ `--expose` 対応がうーん。という感じ。

## 作ったコマンドの例

```console
$ python multihand.py 01*.py hello
hello world
$ python multihand.py 01*.py bye --name bye!
bye bye!
$ python multihand.py 01*.py -h || exit 0
usage: multihand.py [-h] {hello,bye} ...

optional arguments:
  -h, --help   show this help message and exit

subcommands:
  {hello,bye}
    hello      hello
    bye        bye
$ python multihand.py 01*.py bye -h
usage: multihand.py bye [-h] [--name NAME]

optional arguments:
  -h, --help   show this help message and exit
  --name NAME  (default: 'world')
```

重要なのは[元のファイル](01defs.py)が関数定義だけな所。

## 参考: invokeの例

```console
$ cd 00*/ && invoke hello
hello world
$ cd 00*/ && invoke bye --name bye!
bye bye!
$ cd 00*/ && invoke --list || exit 0
Available tasks:

  bye     bye
  hello   hello

$ cd 00*/ && invoke bye -h
Usage: inv[oke] [--core-opts] bye [--options] [other tasks here ...]

Docstring:
  bye

Options:
  -n STRING, --name=STRING
```
