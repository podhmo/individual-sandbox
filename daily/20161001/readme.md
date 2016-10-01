# python

# python 文字列リテラル

こういう描き方することある。xの方はコメントが書けて便利

```python
x = (
    "foo"  # hmm
    "bar"  # yyy
    "boo"
)
y = """\
- foo
- bar
- boo"""
```

# python asyncio 続き

以前のやつマシにする方法がわかったかも。[こちら](../20160929/example_asyncio/)の続き。

## qiitaにasyncioの便利な記事あったっけ？

実質これだけっぽい。

- [Pythonにおける非同期処理: asyncio逆引きリファレンス - Qiita](http://qiita.com/icoxfog417/items/07cbf5110ca82629aca0)

### 細かなtips

- [ズンドコキヨシ with asyncio - Qiita](http://qiita.com/sharow/items/873cd32fa28f1334bce5)

```python
import asyncio
from collections import deque
from contextlib import closing

dq = deque(maxlen=5)  # maxlen以上の場合は先端が切り捨てられる
with closing(asyncio.get_event_loop()) as loop:
    loop.run_until_complete(do_loop())
```

### 得るところなし

個人的にはということだし。asyncioについてという目で見た場合の話。

- [Python3.6の新機能 - Qiita](http://qiita.com/ksato9700/items/ed839a6db6a671fd31e6)
- [Python3.0からPython3.5での変更点 - Qiita](http://qiita.com/CS_Toku/items/32028e65a8bfa97266d6)
- [Python3.5のasync/awaitを使ってスクレイピング - Qiita](http://qiita.com/yasunori/items/03229bfa161e6dc2ea61)
- [Python3.5から導入されるasyncとawaitでコルーチンを扱う - Qiita](http://qiita.com/Lspeciosum/items/98e05c7495369ab9d102)
- [Python3.5で実装されたasync/awaitを使って軽量スレッドの性能ベンチマーク - Qiita](http://qiita.com/haminiku/items/0aaf87e9a52ed41b60a7)


# python anaconda のこと

- `$HOME/anaconda` 以下に色々インストールされる。
- `$PATH` の先頭に `$HOME/anaconda/bin` が追加される
- (conda管理外で)拡張ライブラリをbuildするタイミングで、何らかのコマンドの実行結果を解析して利用するものは死ぬ可能性がある。
- もちろん、(conda管理内でも)condaのことが考えられていない拡張ライブラリのビルドスクリプトは依存の条件を満たせなくて死ぬ場合がある

## install後に `$PATH` が更新される

anacondaがインストールされると `~/.bash_profile` に以下の様なものがしれっと追加される。(mac)

```bash
# added by Anaconda3 4.2.0 installer
export PATH="/Users/<username>/anaconda/bin:$PATH"
```

以下であればまだマシなのかも？

```bash
alias activate_anaconda='export PATH=$HOME/anaconda/bin:`echo $PATH | sed "s@$HOME/anaconda/bin:@@g"`'
alias deactivate_anaconda='export PATH=`echo $PATH | sed "s@$HOME/anaconda/bin:@@g"`'
activate_anaconda
```

ついでにvirtualenvみたいに。 `$PS1` などを変更すれば良いのかもしれない。
