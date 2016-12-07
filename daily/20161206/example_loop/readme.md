注意 loopの中で `batch.Parallel()` を使うの良いじゃんという気持ちになるけれど。
内部でselectしてctx.Doneしていないので危険な感じ。
batch.Parallel()の実装が内部で`loop.Run()`をする形になっていたら解決する話。


