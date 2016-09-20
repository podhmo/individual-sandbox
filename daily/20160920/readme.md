# python logrusに近いlogger

structlogを使う？

- [Structured Logging for Python — structlog documentation](http://www.structlog.org/en/stable/)

問題: 可変のfieldを定義できない？できる？ LTSV形式で出力できない？

この辺見ると良い

- http://www.structlog.org/en/stable/standard-library.html#suggested-configuration

```
time:2016-09-20T12:40:57.29149432+09:00
level:info
msg:xxx-this-is-message
caller:<file>:<lineno>
source:increfetch

あと自由に追加できる
```
