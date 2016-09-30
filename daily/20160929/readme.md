# python asyncio 並行数制御

多分queueでやれば良いはず? => semaphoreで良いのでは？

- [Pythonにおける非同期処理: asyncio逆引きリファレンス - Qiita](http://qiita.com/icoxfog417/items/07cbf5110ca82629aca0)
- [A example of scrapper using asyncio and aiohttp](https://gist.github.com/madjar/9312452)

## semaphoreだと多段の実行がうまくできない

結局、ensure_future + futureのset_resultをawaitするみたいになった。
callback hellをやっつけられない。

# python moduleに `from xxx import *` する方法どうするのだっけ？

```
D = {"a": "b"}
globals().update(D)
print(a)  # => "b"
```
