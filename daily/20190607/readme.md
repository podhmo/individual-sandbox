## pycomment 良い感じにしたい

## python moduleからresourceを取り出すのどうやるんだっけ？

相対パスを指定して物理ファイルにアクセスしたい。

[`pkg_resources.resource_string()`](https://setuptools.readthedocs.io/en/latest/pkg_resources.html#basic-resource-access) とかあるっぽい？

:warning: いや、これは古い。使うにしても、標準ライブラリに [`importlib.resources`](https://docs.python.org/ja/3/library/importlib.html#module-importlib.resources) がある

> This module provides functionality similar to pkg_resources Basic Resource Access without the performance overhead of that package. This makes reading resources included in packages easier, with more stable and consistent semantics.
>
> The standalone backport of this module provides more information on using importlib.resources and migrating from pkg_resources to importlib.resources.

まぁ3.7で追加されたものなのだけれど。

```python
from importlib.resources import read_text

print(read_text("lib2to3", "Grammar.txt"))
```

なかったらpkg_resourcesを見ても良いんじゃ。。

```python
from importlib.util import find_spec

spec = find_spec("lib2to3")
print(spec.loader.open_resource("Grammar.txt").read())
```

結局importlib_resourcesを使う方が良いかも。

## python importlib relative

importlib.util.resolve_name で相対的なモジュール名を変えられる。
(pyinspectとか自力で作ってなかったっけ？)


## python numpy 100

この辺そういえばやってみたことないな。

- https://github.com/rougier/numpy-100/blob/master/100_Numpy_exercises.md

この辺とかも触ってみると良いのかも


- https://github.com/lettier/3d-game-shaders-for-beginners/tree/3d720fae5b19894ce560351591c960398d8e5e61

## 社会に求められる系の記事の体裁

- https://qiita.com/KanNishida/items/98a04646998d43796833
- https://qiita.com/takuya_tsurumi/items/38f2858221599d5f93bd
- https://qiita.com/G-awa/items/a5b2cc7017b1eceeb002

同じもののをどう書くか？という形で進めるのは面白そうとは思う。

## トップページ的なものが欲しくなってきたかもしれない。個人的な。

ホームポジションというか必ず最初に開く場所みたいなものを
notionとか使ってみるのも良いんだろうか？？
