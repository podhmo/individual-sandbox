# python tinydb

```python
from tinydb import TinyDB


# このタイミングで既にjson fileが生成される。
db = TinyDB("foo.json")

# すぐにinsertされてしまう(idは自動で付加)
db.inert({})
```

## 気になること

- tinydbのjson backendがファイルを生成するタイミングはいつ？
- insertされる時のタイミングはいつ？
- insertされるdataのunique constraints的なものは貼れる？
- dbの削除はどうするの?
- 例えばjsonバックエンドの時に、openして書き込んでいるのはどのタイミング
- verbose loggingのようなことをしたい

### syncのタイミング

read,writeのタイミングで常にstorageのread,writeが呼ばれていそう。
例えばJSONStorageでは、内部的には、毎回fdのopen,closeが呼ばれているわけではないものの0にseekしてwriteなどはしている。

本当にインメモリで良いならMemoryStorageを使うのでも良い。

CachingMiddlewareを使うとマシになる。



# python 作業 tinydbとpandas把握する

- pandas気力が起きない(そもそも用途が違うけれど)
- tinydb productionには絶対使わない。
- sqlite直接 schemaの変更に弱い。絶対productionでは使わない。
