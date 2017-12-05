## python pandas

- isinと言うものの存在を知った
- mapとselectorでやる必要もないかもしれない
- any()は行方向と列方向をaxisでいじれる

便利なページ

- https://qiita.com/makora9143/items/bccfaee35f2774f02c74
- http://sinhrks.hatenablog.com/entry/2015/07/11/223124
- http://postd.cc/python_meets_julia_micro_performance/

実体としてpandasは

```
Dataframe({xs: xs, ys: ys}) みたいなときに、以下の様な形でndarrayで列ごとに持つっぽい。

xs列 ndarray[size]
ys列 ndarray[size]
```

- 行ごとの操作禁止(iterrows()とか遅い)
- object型を使わない
- uniqueでないindexを使わない
- memoryとかやばかったら?daskとか使う

### 行毎と列ごと

例えばどちらが早いかという話。

```
df.apply(lambda s: s['x'] + s['y'], axis=1)

df['x'] + df['y']
```

できないなら `np.vectorize()` を使う。

### uniqueでないindex

```
df.index.is_unique
df.index.is_monotonic_increasing
```

