## streamz

https://github.com/python-streamz/streamz

## python pandasを使った生成

pandasを使ったコードが重たいというところから端を発する感じ。
どういう感じの変換のコードを書こうかな。。前処理っぽいことをしたいのだよなー。

- https://github.com/altair-viz/vega_datasets
- https://www.starlette.io/

そこまで届かなかったな。。

## pandas columnsなどを見る

```python
df.columns
```

```python
dict(zip(df.dtypes.index, df.dtypes.map(str)))
```

## pandas いい感じにaggregate

```python
grouped_df = df.groupby(by=["species"]).agg(
    {
        "sepalLength": ["max", "min", "mean", "std", "count"],
        "sepalWidth": ["max", "min", "mean", "std", "count"],
    }
)
```

## pandas groupbyしたmulti indexをflatに

```python
# flat index
grouped_df.columns = grouped_df.columns.to_flat_index()
grouped_df.columns = grouped_df.columns.map(
    lambda xs: xs[0] if not xs[-1] else "-".join(xs)
)
```
