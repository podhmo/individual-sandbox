# 簡単な集計操作みたいなものは手軽にできるように

numpy,pandas,pythonの間を行ったり来たりはできるようにならないとダメっぽい。

- http://qiita.com/richi40/items/6b3af6f4b00d62dbe8e1

単純な集計方法は頭のなかに入れとかないとっぽい。

- http://pandas.pydata.org/pandas-docs/stable/visualization.html

pandasのplot関連

- http://sinhrks.hatenablog.com/entry/2015/11/15/222543

numpy関連のこと

- http://www.kamishima.net/mlmpyja/

visualizationの簡略的な方法

- http://pandas.pydata.org/pandas-docs/stable/visualization.html

# pandasのdata structure

- Series
- DataFrame

詳しくは[Intro to Data Structures — pandas 0.18.1 documentation](http://pandas.pydata.org/pandas-docs/stable/dsintro.html)

## Series

- dict, ndarray, scalaのどれかから作れる
- vectorize operationなど使えて便利ですね

## DataFrame

- 2Dのdata structure
- indexとcolumnsを指定できるらしい
- DataFrame.assignで新しいcolumnを追加できるっぽい

# pandasの雑な使い方

集計と選択の２つ。とりあえずDataFrameをExcelの1テーブルみたいなイメージで考えれば良いものっぽい。
- [10 Minutes to pandas — pandas 0.18.1 documentation](http://pandas.pydata.org/pandas-docs/stable/10min.html)

## 集計

view

- describe()
- head()
- tail(n)

modify

- sort_index()
- sort_values()


## 選択(selection)

- list likeなindex アクセス, slices
- loc() で範囲選択ができるっぽい。 (loc(index, visible_columns))
- queryっぽいこともできる
- `df[df.A > 0]` とか

## 欠損値の扱い

欠損値の扱いがあるの楽そう。 `np.nan` が欠損値を表すっぽい。

- `df.dropna(how="any")`
- `df.fillna(value=5)`

詳しく

- [Working with missing data — pandas 0.18.1 documentation](http://pandas.pydata.org/pandas-docs/stable/missing_data.html#missing-data)

## merge

- `pd.concat(series)` でmergeできるっぽい
- joinもできそう `pd.merge(xs, ys, on='id')` 違う名前にしたいと起動するんだ？(xs.idとys.x_idをonの条件にしたい)

## append

- list操作と同様の感じでいける？ (`df.append(xs, ignore_index=True)`)

## grouping

- `df.groupby(['A','B']).sum()`

## pivot table

便利

## plotting

- plot()

## read系

- read_csv()
- read_excel()


## memo

- `df.date_ranges("20010101", periods=6)` みたいなのたまに便利かもしれない



