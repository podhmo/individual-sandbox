## python slackerとか使わなくて良い

- https://github.com/slackapi/python-slackclient

asyncioなどにも対応してたっけ？

```console
$ pip install slackclient
slackclient==2.7.2
  - aiohttp [required: >3.5.2,<4.0.0, installed: 3.6.2]
    - async-timeout [required: >=3.0,<4.0, installed: 3.0.1]
    - attrs [required: >=17.3.0, installed: 19.3.0]
    - chardet [required: >=2.0,<4.0, installed: 3.0.4]
    - multidict [required: >=4.5,<5.0, installed: 4.7.5]
    - yarl [required: >=1.0,<2.0, installed: 1.4.2]
      - idna [required: >=2.0, installed: 2.9]
      - multidict [required: >=4.0, installed: 4.7.5]
```

## python asyncio

これどうやって治すんだったっけ？

```
ValueError: a coroutine was expected, got <Task pending name='Task-1' coro=<RTMClient._connect_and_read() running at VENV/lib/python3.8/site-packages/slack/rtm/client.py:330>>
```

こういうときに発生した。

```python
asyncio.run(rtm_client.start(), debug=true)
```

意味的にはcoroutineを期待しているのに、Taskだったということなのだよなー。
それじゃ、Taskは何かと言うと`create_task()`されたやつのはず。

ちなみにこうやると、実行できる。しかしこれは本題ではない。

```python
asyncio.get_event_loop().run_until_complete(rtm_client.start())
```

真面目に見るとこういう定義になっている。

/opt/local/Library/Frameworks/Python.framework/Versions/3.8/lib/python3.8/asyncio/runners.py

```python
def run(main, *, debug=False):
    """Execute the coroutine and return the result."""
# ...

    if not coroutines.iscoroutine(main):
        raise ValueError("a coroutine was expected, got {!r}".format(main))
    loop = events.new_event_loop()
```

あー、そもそもこれslackbot側の実装が微妙なのか。まぁ `run_until_complete()` を使うのが正解そう。

~/vboxshare/venvs/my/lib/python3.8/site-packages/slack/rtm/client.py

```python
class RTMClient(object):
    """An RTMClient allows apps to communicate with the Slack Platform's RTM API."""
# ...

    def start(self) -> asyncio.Future:
        """Starts an RTM Session with Slack."""
# ...

        if os.name != "nt" and current_thread() == main_thread():
            signals = (signal.SIGHUP, signal.SIGTERM, signal.SIGINT)
            for s in signals:
                self._event_loop.add_signal_handler(s, self.stop)
        future = asyncio.ensure_future(self._connect_and_read(), loop=self._event_loop)
        if self.run_async:
            return future
        return self._event_loop.run_until_complete(future)
```
