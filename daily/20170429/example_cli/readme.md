# pythonでコマンドラインアプリケーション作る話

これの記事を見てなるほど～と思ったついでに自分ならどうするか考えてみた。

- [Pythonでコマンドラインアプリケーションを作るときの雛形 - Qiita](http://qiita.com/Alice1017/items/0464a38ab335ac3b9336)

memo:

- コマンドラインアプリケーションは、setup.py経由でインストールされるcliのコマンドのことを指しているっぽい
- 主題はmainをどうやって書くかという話

## 提案していること

提案しているのはどういうものがあるかというと、以下のようなもの。

- parserをfoo.cliに書いてimport
- setup.pyのconsole_scriptsからは `foo.__main__:main` を呼び出す
- 終了statusでexit
- エラーメッセージを短くする(default)
- 全部表示させたいときの `--stack-trace` オプションを追加

### parserをfoo.cliに書いてimport

これは自分の場合はしないかも、理由はargparseのparesrは他で使うことがないので。終了statusでexitと含めてこういう感じに書くことが多いかも。

```python
from foo import do_something

def run(x, y, z):  # 引数は適切なもの
    # いつもはここで直接sys.exitしていた
    return do_something(x, y, z)


def main():
    import argparse
    parser = argparse.ArgumentParser()

    parser.add_argument("-x")
    parser.add_argument("-y")
    parser.add_argument("-z")

    args = parser.parse_args()
    # 今までしていなかったけれど。ここでsys.exitするのは良いかも？
    sys.exit(run(args.x, args.y, args.z))
```

### setup.pyのconsole_scriptsからは `foo.__main__:main` を呼び出す

これは純粋になるほどなーと思った。1パッケージ1コマンドにするならこれもありかも。というのも `__main__` で書くということは `python -m foo` でも呼べるといことなので。

個人的には、いつも `foo/cli.py`, `foo/cmd.py`, `foo/command.py`, `foo/__init__.py` あたりに `main()` を書いていた

### エラーメッセージを短くする(default)

これはいつもしないことが多かった。常にstack trace出た方が良いじゃんという立場だったりもしたけれど。エラー時にコンソールにいっぱい出力される系のコマンドやっぱりうざかったりする？のかもしれない。自分ならこういう感じに書くかも。エラーの型は直接クラスから取ってくる。あと、例外に `__str__()` が実装されている場合を尊重して直接eをprintする。

```python
# これを
def main():
    # .. snip
    sys.exit(run(args.x, args.y, args.z))

# こうする
def main():
    # .. snip
    try:
        sys.exit(run(args.x, args.y, args.z))
    except Exception as e:
        sys.stderr.write("{e.__class__.__name__}: {e}".format(e=e)
        sys.exit(1)
```

あと、これと一緒にtrace back表示するオプションをつけるとしたら。 `--debug` が付いていたらstack traceを表示にしちゃうかも。その場合は単にraiseしちゃう。

```python
def main():
    # .. snip
    parser.add_argument("--debug", action="store_true", default=False)
    # .. snip
    try:
        sys.exit(run(args.x, args.y, args.z))
    except Exception as e:
        if args.debug:
            raise
        print("{e.__class__.__name__}: {e.args[0]}".format(e=e), file=sys.stderr)
        sys.exit(1)
```

## 実際の例

```bash
$ make
python cli.py  || echo "err"
10 / 2 = 5.0

python cli.py -x 10 -y 3  || echo "err"
10 / 3 = 3.3333333333333335

python cli.py -y 0  || echo "err" # error
ZeroDivisionError: division by zero

err

python cli.py -y 0  --debug  || echo "err"  # error(verbose)
Traceback (most recent call last):
  File "cli.py", line 31, in <module>
    main()
  File "cli.py", line 22, in main
    sys.exit(run(args.x, args.y))
  File "cli.py", line 9, in run
    div(x, y)
  File "cli.py", line 5, in div
    print("{} / {} = {}".format(x, y, x / y))
ZeroDivisionError: division by zero
err
```
