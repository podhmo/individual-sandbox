## python literal python的なものを作りたい

jupyter notebook

- 利点はcellがあること
- 欠点はcellがあること

欲しい機能

- python code to ipynb file translator

欲しい機能(詳細)

```python
with code():
    x = 1
    y = 2
    x + y

with markdown() as m:
    m.write("""this is sample script.""")

with markdown() as m:
    m.write("this is sample script.")
    m.write("it enables to write text with markdown format.")

```

こんな感じのコードが以下みたいな形になって欲しい。

```
## python code cell

x = 1
y = 2
x + y

## markdown cell

this is sample script.

## markdown cell

this is sample script.

it enables to write text with markdown format.
```

ゆくゆくは以下の様なコードが同様の形にコンパイルされると嬉しい。

```python
with code():
    x = 1
    y = 2
    x + y

"""
this is sample script.
"""

or

# this is sample script.
# it enables to write text with markdown format.
```

とは言え、ふつうにpythonのインタプリタで実行もしたい。

### 気にしたいこと

- 標準出力・標準エラー
- 末尾の結果を出力(特にグラフ)
- (数式)
