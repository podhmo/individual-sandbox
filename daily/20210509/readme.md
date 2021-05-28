## python あとで初期化するproperty

```
ob = Object()
ob.name  # => <undefined of "name">

ob.materialize()
ob.name = "foo"
ob.name  # => "foo"
```

### 追記

descripterでやれないことはない。
ただ、dataclassesみたいなものと相性が悪いかもしれない。
(`__repr__()`を手軽にしたくなると`__get__()`が呼ばれてしまう。一方でvarsにするのも`__init__()`を作るのも辛い)
