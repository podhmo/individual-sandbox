#[python][moduleknife]pipenvの起動を早くしたという話

pipenvの起動を早くしたという話です。良い話ですね。

https://dev.to/methane/how-to-speed-up-python-application-startup-time-nkf

ところでリンク先の記事はpython3.7で導入されるPYTHONPROFILEIMPORTTIMEの紹介なのですが。古の民(3.7以前の人々)はどのように解決していたのでしょうか？似たようなことを昔やっていたことがあり。その時は[import hook](https://docs.python.jp/3/reference/import.html?highlight=finder%20import#import-hooks)で無理矢理頑張る方法をとったりしていました。

## modulegraph

完全には同じ事はできないのですが。import hookを可能な限り記録可能なものに置き換えて実行するみたいな感じです。

以前書いたmoduleknifeというrepositoryのmodulegraphというコマンドで似たようなことをしています。

https://github.com/podhmo/moduleknife

もちろんpython(pure python)のレイヤーでのimport hookの置き換えなので、素のまっさらなpythonが立ち上がるまでの部分は計測不能なのですが。通常のライブラリやアプリを作る上で素のまっさらなpythonが立ち上がる時間自体はどうやっても早くできない固定コストなので無視してます。

## pipenvのimport timeの高速化の部分について

同じ事ができるか実際に試してみると。

00import.py

```python
import pipenv
```

modulegraphというコマンド経由でpythonスクリプトを実行すると、ここでgraphviz用のdotファイルが生成されます。

```
$ modulegraph --metadata=time --outfile=./00.dot 00import.py
$ svg -Tsvg 00.svg > 00.svg
```

ちなみに.dotファイルの中のコメントを見ると元の記事の通りにIpythonとpkg_resource関連が怪しいということがわかります(実行にかかった時間で降順でsort)。

```
// load ~/my/lib/python3.6/site-packages/pipenv/__init__.py ... 0.5924477577209473s
// load ~/my/lib/python3.6/site-packages/pipenv/cli.py ... 0.5918803215026855s
// load ~/my/lib/python3.6/site-packages/pipenv/patched/dotenv/__init__.py ... 0.2650315761566162s
// load ~/my/lib/python3.6/site-packages/pipenv/patched/dotenv/ipython.py ... 0.2640550136566162s
// load ~/my/lib/python3.6/site-packages/IPython/__init__.py ... 0.2633070945739746s
// load ~/my/lib/python3.6/site-packages/IPython/terminal/embed.py ... 0.23813724517822266s
// load ~/my/lib/python3.6/site-packages/IPython/terminal/interactiveshell.py ... 0.1785898208618164s
// load ~/my/lib/python3.6/site-packages/pipenv/patched/pip/__init__.py ... 0.17690563201904297s
// load ~/my/lib/python3.6/site-packages/pipenv/patched/pip/utils/__init__.py ... 0.11338138580322266s
// load ~/my/lib/python3.6/site-packages/pipenv/patched/pip/_vendor/pkg_resources/__init__.py ... 0.10068082809448242s
// load ~/my/lib/python3.6/site-packages/pkg_resources/__init__.py ... 0.0988779067993164s
// load ~/my/lib/python3.6/site-packages/urllib3/__init__.py ... 0.045993804931640625s
```

補足するとこの時間は例えば `foo` パッケージから `bar` パッケージを読むという構造になっていた場合に以下の様な形で計測されます。

```
tf0 = import fooの前の時刻
  import foo
  tb0 = import barの前の時刻
    import bar
  tb1 = import barの後の時刻
tf1 = import fooの後の時刻

このとき
import fooの時間は tf1 - tf0
import barの時間は tb1 - tb0
```

これだけでも記事と同様にpkg_resourceの読み込みとdotenv経由のipythonの読み込みが遅いということはわかります。
ついでにgraphvizでグラフを生成してみましょう。すごく大きくて何だかわからないグラフが表示されます(蛇足ですが画像ではなくsvgの場合検索が効くのが便利です)。


