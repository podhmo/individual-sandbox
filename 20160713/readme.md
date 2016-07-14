# propertyに新たにinstance変数として状態を代入できないはず？

できないはず

```
AttributeError: can't set attribute
```

# pandasとか弄る気になった

ついでにipython notebook越しに使ってみよう
%matplotlib inline

```
pip install jupyter pandas matplotlib
```

## pandas

便利な理由

- 共通のinterface
- shortcut function
- 欠損値の取り扱い

便利

- `describe()`
- `plot()`  # kind: bar, pie,
- matplotlibと組み合わせると状況確認しやすい

## notebook

cellの部分

- shift + enter -- 新しいcell作成
- ctrl + enter -- 評価
- enter -- 何もせずに改行


## install

```bash
pip install jupyter pandas
```

## daily life

```bash
jupyter notebook
```

ここから先全部browser


browser立ち上げたくない場合

```bash
jupyter notebook --no-browser
```

## 何か色々試したい場合

extensions

```bash
jupyter nbextension list
jupyter nbextension install <extension>
```

kernels

```bash
jupyter kernelspec list
jupyter kernelspec install <kernel>
```

## その前にjupyterのこと

- [Overview — Jupyter Documentation 4.1.1 alpha documentation](http://jupyter.readthedocs.io/en/latest/index.html)
- [他のkernel入れたい場合](https://github.com/ipython/ipython/wiki/IPython-kernels-for-other-languages)
