# monogusa

いろいろ考えてみる。手触りのようなものが得られないのが困るのだよなー。
変換と手触りとの間のなにか。

## reactions

- eventsをeventsとreactionsに分けたのは良かった
- replierというprotocolではなくreplyという関数にしちゃうのも便利

型の引数を間違えたときにはmypyでチェックされてほしい。

```console
$ mypy --strict 00echo.py --pretty
00echo.py:6: error: Argument 1 has incompatible type "Callable[[MessageEvent[Any], reply], None]";
expected "Reaction"
    @events.subscribe(events.MessageEvent[t.Any])
     ^
00echo.py:8: error: Too few arguments for "__call__" of "reply"
        reply(f"got: {ev.content}")
        ^
00echo.py:8: error: Argument 1 to "__call__" of "reply" has incompatible type "str"; expected
"MessageEvent[<nothing>]"
        reply(f"got: {ev.content}")
              ^
Found 3 errors in 1 file (checked 1 source file)
```

そういえばprotocolのoverloadってできたっけ？

### overload


欲しいreactionって？

- react
- send # with channel
- reply
- direct_reply

### もう少し考えたほうが良さそう

- inputとoutputで使いたいものが違ったりしない？
- workerはconsoleのeventを取り出してoutputはslack?

これは違って、consoleのeventではなくslackのeventを渡す？
(ということはMessageオブジェクトを作らなければいけないのでは？)
