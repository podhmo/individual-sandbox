## pythonでのLSP

理解のために把握しておいたほうが良いこと

- palantir/python-language-server

  - pytest-dev/pluggy
  - palantir/python-jsonrpc-server
  - jsonrpc(JSONRPC 2.0)
  - (jedi)
  - (rope)

- LSP
- eglot(editor)

残り

- virtualenvとの対応
- uri.rootUriの関係
- traceする方法

他の作業

- poetry使う
- pyinspectを更新
- eglotの設定
- cookiecutterに乗り換え


基本的にはJSONなどでやるのが良いと思います。通常は。ただJSONなどがツライのは、例えばmath.nanなどを含んだようなデータ（例えば計算中の一時データ）とか、単純なobjectの大きめのツリーなどのときですね（例えばASTのnodeオブジェクトとか)。

具体的には、並行処理でMultiProcessExecutorを使う時に、各forkしたprocessへのデータの受け渡しがpickleです。

あと、ちょうど最近pythonのlsp(language server protocol)への対応のライブラリを見ていたのですが、そこでのコードの補完にはjediというライブラリが使われていて、このjediは評価器でコードを解析するタイミングでsubprocessを立ち上げるんですが(壊れたコードなどを渡してプログラム全体がエラーになったら嬉しくないのでprocessを分ける)、そこでのデータの受け渡しにpickleが使われています。


## python 謎のエラー

jediでsubprocessを作っているがそれらが終了されていないっぽい？

```
/usr/lib/python3.7/subprocess.py:852: ResourceWarning: subprocess 31056 is still running
sys:1: ResourceWarning: unclosed file <_io.BufferedWriter name=4>
sys:1: ResourceWarning: unclosed file <_io.BufferedReader name=5>
sys:1: ResourceWarning: unclosed file <_io.BufferedReader name=7>
```

- subprocessがkillなどされない
- subprocessのpipeが残る？

ちなみにjediがsubprocessを作っているのは以下の部分。

jedi/evaluate/imports.py

```python
from jedi.evaluate import compiled

class Importer(object):
    def _do_import(self, import_path, sys_path):
# ...
        module = _load_module(
            self._evaluator, module_path, code, sys_path,
            module_name=module_name,
            safe_module_name=True,
        )


def _load_module(evaluator, path=None, code=None, sys_path=None,
                 module_name=None, safe_module_name=False):
# ...
            module = compiled.load_module(evaluator, path=path, sys_path=sys_path)
```

動いているsubprocessはこれ

```python
class GeneralizedPopen(subprocess.Popen):
    def __init__(self, *args, **kwargs):
        if os.name == 'nt':
            try:
                # Was introduced in Python 3.7.
                CREATE_NO_WINDOW = subprocess.CREATE_NO_WINDOW
            except AttributeError:
                CREATE_NO_WINDOW = 0x08000000
            kwargs['creationflags'] = CREATE_NO_WINDOW
        super(GeneralizedPopen, self).__init__(*args, **kwargs)
```

subprocess.pyの方の`__exit__`の定義こうじゃん。

```python
    def __exit__(self, exc_type, value, traceback):
        if self.stdout:
            self.stdout.close()
        if self.stderr:
            self.stderr.close()
        try:  # Flushing a BufferedWriter may raise an error
            if self.stdin:
                self.stdin.close()
        finally:
            if exc_type == KeyboardInterrupt:
                # https://bugs.python.org/issue25942
                # In the case of a KeyboardInterrupt we assume the SIGINT
                # was also already sent to our child processes.  We can't
                # block indefinitely as that is not user friendly.
                # If we have not already waited a brief amount of time in
                # an interrupted .wait() or .communicate() call, do so here
                # for consistency.
                if self._sigint_wait_secs > 0:
                    try:
                        self._wait(timeout=self._sigint_wait_secs)
                    except TimeoutExpired:
                        pass
                self._sigint_wait_secs = 0  # Note that this has been done.
                return  # resume the KeyboardInterrupt

            # Wait for the process to terminate, to avoid zombies.
            self.wait()
```

## 作業

- async dispatcher(jsonrpc-server)
- pyinspectのrefactoring

## python asyncなdispatcher

python-jsonrpc-serverにasync対応したかったりする。
dispatcherでsleepを1.0s掛けた結果を気にしたい。

## python jsonrpc-server

endpointのworkersの数の対応ってどうなっていたんだっけ？
なるほどThreadPoolExecutorを使っていたのか。callbackだった。
shutdownを忘れずに(`__exit__`)

## xxx:

そういえばchildからparent processへのhealth check
