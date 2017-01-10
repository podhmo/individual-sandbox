# python signal handlingのテスト

signalのhandling自体は以下で簡単にできる

```python
import signal
import sys


def on_sigint(signum, frame):
    print("hmm")
    sys.exit(1)

signal.signal(signal.SIGINT, on_sigint)
```

これが確実にtrapされたことをテストにするにはどうするかという話。わりとだるい。

[現状](./example_signal/00test.py)はmultiprocessで立ち上げterminate(SIGTERM)させるみたいな感じ。

面倒くさい点

- main thread 以外でsignalをtrapできない -> multiprocessingを使う
- どうにかsignalを贈りたい -> process作ってterminate()
- 別のprocessと通信しなくては呼ばれたことを保証できない -> Queueを使う
- 任意のcallbackを呼びたい -> _on_trapという抜け穴を作る
