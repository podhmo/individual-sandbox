## python logger.infoがだるい

printがlogger.infoになれば良くない？
module globalだと `from tinylog import print` したときにnameが固定されちゃうな。

頑張ってcallstackを読む必要はないかも？
さすがにmodule hookでASTを取り出して変換するのはバカバカしい。

`handofcats` 辺りにあれば良さそう。

## python subprocess

綺麗にまとまっているけれど。新し目の情報とかがないかも。asyncioとかmultiprocessing.shared_memoryとか

- https://qiita.com/kaitolucifer/items/e4ace07bd8e112388c75
- https://docs.python.org/3/library/multiprocessing.shared_memory.html

