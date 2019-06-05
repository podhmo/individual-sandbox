## python 良い感じに読み込んだ場所を表示するYAML

- [../20190515/example_loading](../20190515/example_loading)

## python 大きなJSONでも状況が把握できるような出力

- jsonknife describe ?
- verboseな出力は避けたい
- 何かで調整可能にする?(深さ?)
- 値をそのままではなくkeysにする？
- 全体の大きさを表示する？

## python methodが渡された時にそのmethodのownerの情報ってみれたっけ？

- `method.__self__`

あと `method.__qualname__` とかも便利

```python
    @reify
    def fnnames(self, fns) -> t.List[str]:
        r = []
        for fn in fns:
            if hasattr(fn, "__self__"):
                r.append(f"{fn.__self__.__class__.__module__}:{fn.__qualname__}\n")
            else:
                r.append(f"{fn.__module__}:{fn.__qualname__}\n")
        return r
```

see also

- https://docs.python.org/ja/3/reference/datamodel.html

## emacs flymake, flycheckに適した形式

- flymake
- flycheck

## python openapi-stream

### 試したいこと

- flymake, flycheckで表示できる形式
- テキトーなconfに対してのvalidation
- visitorのhistoryの保存

### 直したいこと

- どのoneOfでもないというようなエラーを出したい。
- caseが持つ条件を手軽にみたい

### とりあえずの目標

- petstore.jsonを完走

