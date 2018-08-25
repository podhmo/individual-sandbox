## python pandas DataFrameの作り方

- 速度の問題
- 自由度の問題
- メモリー消費の問題

- dtype指定
- dtypeのdowncast
- object/S1/category

- concatのコスト

- chunksize,nrows(parsers)

ndarrayの作成

- reshape
- zerosにsize

## python pandas memory消費

```
df.info(memory_usage='deep')
```

- https://www.dataquest.io/blog/pandas-big-data/

## python pandas Dataframeの作り方

pandas.core.dataframe:DataFrame

- `__init__`
- from_csv -- deprecated
- from_dict
- from_items -- deprecated
- from_records

Seriesとかの作り方も知っておくと。。
効率がマシなのはどれだろ。

ただ以下の様なメッセージがある

> The pandas.core, pandas.compat, and pandas.util top-level modules are PRIVATE. Stable functionality in such modules is not guaranteed.

https://pandas.pydata.org/pandas-docs/stable/api.html

そう考えると、pandas.read_xxxを使うのが良いのかも？

### 追記

from_recordsは遅い。`[row,row,row]` 的な表現なので。

### hmmm

memo


- 行に対するループ / DataFrame.apply は 使わない
- object 型は使わない / category使う / 不要ならdowncastを
- ユニークでない / ソートされていない index は使わない

たぶんそこそこ巨大ならparquetが良い?。(feather?)

categorical dataが。。

### performanceのこと

- https://tech.blue-yonder.com/efficient-dataframe-storage-with-apache-parquet/
- https://github.com/wesm/feather/issues/188
- https://stackoverflow.com/questions/48083405/what-are-the-differences-between-feather-and-parquet
- https://qiita.com/tamagawa-ryuji/items/3d8fc52406706ae0c144
- https://qiita.com/tamagawa-ryuji/items/deb3f63ed4c7c8065e81
- https://blog.amedama.jp/entry/2018/07/11/081050
- https://mikebird28.hatenablog.jp/entry/2018/05/12/213740

概ねファイルサイズの10倍のメモリ

とりあえず、全部parquetで良いじゃんみたいな気持ちでいたけれど。categoicalデータの対応が微妙らしい(雑に言えば文字列などではなくenumをみたいな感じ)。

### util

- http://spitta8823.hatenablog.com/entry/2017/06/16/143856
- http://spitta8823.hatenablog.com/entry/2017/06/14/025442
- https://blog.amedama.jp/entry/2018/06/21/235951
- https://blog.amedama.jp/entry/2018/07/23/084500
