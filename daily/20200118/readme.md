## python 親が死んだら死ぬ子process

たしかpidを受け渡す感じだったはず。

- https://github.com/palantir/python-language-server
- https://github.com/davidhalter/jedi

それはともかくたしかjediはsubprocessを使っていたはず

### jedi

この辺りかな。

- https://github.com/davidhalter/jedi/blob/master/jedi/inference/compiled/subprocess/__init__.py

subprocess.Popenを拡張したオブジェクトを作っている。
そしてstdoutを取得する関数をbackground threadとして起動している。

すなおに一行ずつ取り出してqueueに詰める関数っぽい。

```python
def _enqueue_output(out, queue):
    for line in iter(out.readline, b''):
        queue.put(line)
```

どうもLinsterがcompanion object?

受け取ったがわはstdinをpickle.load()している。
そいつを内部の_run()に渡して結果をpickle.dump()している。

_runは、関数か、interface_statesというところから取り除くか、access handlersからオブジェクトを取り出して関数として実行。interface_statesというのはstate holderっぽい。

あー、受け取るやつを無限ループしているのかsubprocessは。なるほど。

### pyls

https://github.com/palantir/python-language-server/blob/5d01b627306b44f605c79350e44f819f3307cfca/pyls/__main__.py#L33-L38

そうそう。check parent process。

https://github.com/palantir/python-language-server/blob/5d01b627306b44f605c79350e44f819f3307cfca/pyls/python_ls.py#L214-L225

ここか。process not aliveならexitを呼んでいる。そしてthreading.Timer()で定期的にループ。

```python
    def is_process_alive(pid):
        """Check whether the process with the given pid is still alive.

        Args:
            pid (int): process ID

        Returns:
            bool: False if the process is not alive or don't have permission to check, True otherwise.
        """
        if pid < 0:
            return False
        try:
            os.kill(pid, 0)
        except OSError as e:
            return e.errno == errno.EPERM
        else:
            return True
```

0をkillで送るのは監視の常套句。

- http://r9.hateblo.jp/entry/2018/01/15/193444

OSErrorのEPERMってなんだっけ？permission?

- https://qiita.com/h2suzuki/items/0cc924cdd9d5c6d47448

どうやら10秒ごとにkillで死活監視することになっている模様。

## python argparse Dict

dictの扱いをどうするか。設定ファイルの扱いをどうするか。

## python argparse

丁寧にhelpも対応したバージョンを書いてあげるか。

- https://pod.hatenablog.com/entry/2018/08/13/231332

### 追記

- https://pod.hatenablog.com/entry/2020/01/18/153058

## aws sqs

せっかくだし。記事にしよう。
