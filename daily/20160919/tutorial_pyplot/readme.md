これを実行できるようにしたい

- [Pyplot tutorial — Matplotlib 1.5.3 documentation](http://matplotlib.org/users/pyplot_tutorial.html)


# setup

- [この辺](https://github.com/podhmo/individual-sandbox/tree/master/daily/20160916#mac-numpy%E3%81%A8%E3%81%8B%E3%81%AE%E7%92%B0%E5%A2%83)

# tutotial

- 00line.py -- 線を書くだけ
- 01axis.py -- X軸Y軸の範囲を決める
- 02nparray.py -- numpyのarrayを使う
- 03multi-figure.py -- 複数の図を表示する
- 04with-text.py -- テキストの表示
- 05with-annotation.py -- annotation
- 06nonliner-axis.py -- 対数グラフなど線形以外の座標のグラフ

# 線の見た目に関する修飾

- [Pyplot tutorial — Controlling line properties](http://matplotlib.org/users/pyplot_tutorial.html#controlling-line-properties)
- [lines — Matplotlib 1.5.3 documentation](http://matplotlib.org/api/lines_api.html#matplotlib.lines.Line2D)

# memo

jupyter notebookで実行する時にはこれを忘れないようにしないと

```
%matplotlib inline
```

```python
pylot.plot(xs, ys, 'ro')  # この 'ro' は何者？
```

rは赤色で。oは何だろ。

figure,plotの引数の意味

```python
plt.figure(1)                # the first figure
plt.subplot(211)             # the first subplot in the first figure
plt.plot([1, 2, 3])
plt.subplot(212)             # the second subplot in the first figure
plt.plot([4, 5, 6])


plt.figure(2)                # a second figure
plt.plot([4, 5, 6])          # creates a subplot(111) by default

plt.figure(1)                # figure 1 current; subplot(212) still current
plt.subplot(211)             # make subplot(211) in figure1 current
plt.title('Easy as 1, 2, 3') # subplot 211 title
```

grid表示したい

```python
plt.grid(True)
```

数式の表現を使いたい => $で囲む

```python
plt.title(r'$\sigma_i=15$')
```
