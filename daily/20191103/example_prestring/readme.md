# prestringの昔に作ったconfig cache

これは元々scaffold用のproject templateを1ファイルで配信したいと言うような時に考えた仕組み。
設定をどうやって持つかと言うような話。

- 明示的に設定を持つ
- 実行を元に設定を作成する

これの後者をイメージしていたのだけれど。以下の点で面倒。

- 実行してみないとわからない
- 何回たずねられるかわからない (自動化し辛い）

ただし以下の点で便利

- 保持しうる設定のうち必要最小限のものだけがたずねられる

### 追記

今だったらどうやってつくるんだろう？

- 明示的ならdataclasses辺りからの変換でつくるかも？
- 暗黙的ならmock.Mockのような形でscanできて欲しい

後者はちなみに設定を元にした条件分岐が無理。

## 明示的な方

どういうことを気にしようか

- 名前とhelp message (description) が持てる
- dictになる
- `--use-default`, `--no-input` みたいな形で全くの入力なしで実行できる
- 実際の実行を避けてdry runができる

```
python 00*.py 00foo
package (package name)['foo-bar']:
version (version)['0.0.0']:
INFO:__main__:[f] create: 00foo/foo-bar/.gitignore
INFO:__main__:[f] create: 00foo/foo-bar/README.rst
INFO:__main__:[f] create: 00foo/foo-bar/CHANGES.rst
INFO:__main__:[f] create: 00foo/foo-bar/foo-bar/__init__.py
INFO:__main__:[f] create: 00foo/foo-bar/foo-bar/tests/__init__.py
INFO:__main__:[f] create: 00foo/foo-bar/setup.py
```
