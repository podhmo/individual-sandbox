## visualize

- 累積したヒストグラムよりも，基本指針に従って各ラベルごとにプロットしたほうが見やすい
- ヒートマップは確認に便利？ (pair plotを潰した表示が見れる)
- pair plotの表示はどうやる？

https://yakst.com/ja/posts/5485

これが一番役に立つのかもしれない。

### scatter matrix

どうやらpdvegaというパッケージがあるらしい。これどれくらいまともなんだろう？

- https://stackoverflow.com/questions/50919267/pairwise-scatterplot-matrix
- https://altair-viz.github.io/pdvega/plotting.html#scatter-matrix

```
import altair as alt
from vega_datasets import data
import pdvega

iris = data.iris()
pdvega.scatter_matrix(iris, "species", figsize=(7, 7))
```

これ上手く動かないな。

```
AttributeError: module 'pandas.core' has no attribute 'index'
```

そもそもarchiveされていた。

> This repository has been archived by the owner. It is now read-only. 

### 自分でscatter matrixを書いた方が良いのでは？

```
import altair as alt
from vega_datasets import data

source = data.cars()

alt.Chart(source).mark_circle().encode(
    alt.X(alt.repeat("column"), type='quantitative'),
    alt.Y(alt.repeat("row"), type='quantitative'),
    color='Origin:N'
).properties(
    width=150,
    height=150
).repeat(
    row=['Horsepower', 'Acceleration', 'Miles_per_Gallon'],
    column=['Miles_per_Gallon', 'Acceleration', 'Horsepower']
).interactive()
```

columnに素直にcolumnを書いている。そして、rowにもcolum名を書いている。scatter matrixなので当然か。

```
Name 	Miles_per_Gallon 	Cylinders 	Displacement 	Horsepower 	Weight_in_lbs 	Acceleration 	Year 	Origin
0 	chevrolet chevelle malibu 	18.0 	8 	307.0 	130.0 	3504 	12.0 	1970-01-01 	USA
1 	buick skylark 320 	15.0 	8 	350.0 	165.0 	3693 	11.5 	1970-01-01 	USA
2 	plymouth satellite 	18.0 	8 	318.0 	150.0 	3436 	11.0 	1970-01-01 	USA
3 	amc rebel sst 	16.0 	8 	304.0 	150.0 	3433 	12.0 	1970-01-01 	USA
4 	ford torino 	17.0 	8 	302.0 	140.0 	3449 	10.5 	1970-01-01 	USA
... 	... 	... 	... 	... 	... 	... 	... 	... 	...
401 	ford mustang gl 	27.0 	4 	140.0 	86.0 	2790 	15.6 	1982-01-01 	USA
402 	vw pickup 	44.0 	4 	97.0 	52.0 	2130 	24.6 	1982-01-01 	Europe
403 	dodge rampage 	32.0 	4 	135.0 	84.0 	2295 	11.6 	1982-01-01 	USA
404 	ford ranger 	28.0 	4 	120.0 	79.0 	2625 	18.6 	1982-01-01 	USA
405 	chevy s-10 	31.0 	4 	119.0 	82.0 	2720 	19.4 	1982-01-01 	USA
```

### あとでirisをいじってみる

```
alt.Chart(source).mark_circle().encode(
    alt.X(alt.repeat("column"), type='quantitative'),
    alt.Y(alt.repeat("row"), type='quantitative'),
    color='species:N'
).properties(
    width=150,
    height=150
).repeat(
    row=["sepalLength", "sepalWidth", "petalLength", "petalWidth"],
   column=["sepalLength", "sepalWidth", "petalLength", "petalWidth"],
).interactive()
```

普通に簡単やな。あとはheatmapをどうやって使うんだろう？
ただ、XとYが同じときには分布を出して欲しいな。

### correlation ratioのheatmapは？

ここでまだマージされていない。

```
import altair as alt
import pandas as pd
from vega_datasets import data

## Section 1: Correlation plot

# Load the data
df_iris = data.iris()

corrMatrix = df_iris.corr().round(2).reset_index().rename(columns = {'index':'Var1'}).melt(id_vars = ['Var1'],
                                                                                                value_name = 'Corr',
                                                                                                var_name = 'Var2')

# Create the heatmap first
heatmap = alt.Chart(corrMatrix).mark_rect(
).encode(
    alt.X('Var1:O', title = ''),
    alt.Y('Var2:O', title = '', axis=alt.Axis(labelAngle=0)),
     alt.Color('Corr:Q',
                scale=alt.Scale(scheme='viridis'))
)

# Add the correlation values as a text mark
text = heatmap.mark_text(baseline='middle', fontSize=20).encode(
    text=alt.Text('Corr:Q', format='.2'),
    color=alt.condition(
        alt.datum['Corr'] >= 0.95,
        alt.value('black'),
        alt.value('white')
    )
)

# Set the height, width, title and other properties
corrMatrix_chart = (heatmap + text).properties(
    width = 400,
    height = 400,
    title = "Iris variables correlation matrix",
)
corrMatrix_chart.configure_axis(
    labelFontSize=18,
    titleFontSize=24,
).configure_title(
    fontSize=24,
    anchor='start',
).configure_legend(
    labelFontSize=20,
    titleFontSize=20)
```

- https://github.com/altair-viz/altair/pull/1945

## python altair いじるか

この辺を追えるように

- https://qiita.com/tomo_makes/items/4d69f347a5e49346df37
- https://qiita.com/keiono/items/9042bf58224ca54bdb45

### tutorial

とりあえず、hello worldができるような状況までは持っていきたい。
notebookでの試行錯誤の方が都合がよさそうなのでその辺も含めて。

- https://altair-viz.github.io/

全部の使い方を覚える必要はないかも。ここを見て使いたい視覚表現のコードを読んでいくというのが無難そう。

https://altair-viz.github.io/gallery/index.html

欲しいのはどういうものだろう？

- 時系列的なカウント
- 分布

あと、個別のグラフではなく一気に表示するようなものも欲しい。
探せばvoiolin plotもあるんだなー。

https://altair-viz.github.io/gallery/violin_plot.html

scatter matrixもある

https://altair-viz.github.io/gallery/scatter_matrix.html

### 大まかな流れ

- Chartオブジェクトを作り
- transform_density()とかを呼び
- mark_area()を呼び
- encode()を呼ぶ。

ところでinteractiveの実装などはどうなっているんだろう？

### misc

こういう設定を書いておくと嬉しいらしい。後で調べる。universal color schemaってどう良いんだろう？

```
# 初期化
import altair as alt
alt.renderers.enable('notebook')  # jupyter notebook の場合のみ初期化に必要
# ユニバーサルカラースキーマ作成
scale_color_cud = alt.Scale(range=cud_rgb_hex)
# 上限エラー無効化
alt.data_transformers.enable('default', max_rows=None)
```

https://qiita.com/s_katagiri/items/26763fd39f3dd9756809

もしかして、pdvegaとかは不要？

```
hists = []
chart = alt.Chart(df_large.iloc[:500]).mark_area(opacity=0.3, interpolate='step')

for col in ['num_1', 'num_2', 'num_3', 'cat_1', 'cat_2_ja']:
    hist = chart.copy().encode(
        alt.X(col, bin=alt.Bin(maxbins=50)),
        alt.Y('sum(freq):Q', stack=None, axis=alt.Axis(format='.0%'), title='relative freq.'),
        alt.Color('is_train:O', scale=scale_color_cud)
    ).transform_window(
        total='count({})'.format(col),
        frame=[None, None],
        groupby=['is_train']
    ).transform_calculate(
        freq='1/datum.total'
    ).properties(width=150, height=150).interactive()
    if df_large[col].dtype in [int, float]:
        hists.append(hist)
    elif df_large[col].dtype == 'O':
        hists.append(hist.encode(alt.X(col)))
alt.hconcat(*hists)
```

## python notebook

公開されているrepositoryならnbviewerが使えるのだよなー。

https://nbviewer.jupyter.org/

とりあえず覚えておきたいのはなんだろう？

command mode

- bでcellの追加
- mでmarkdown mode
- xでcut

edit mode

- ESCでcommand modeへ
- shift + enterで実行 + cellの追加
- ctrl + shift + '-' でcellを分割
