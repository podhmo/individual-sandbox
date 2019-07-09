## python poetryを試す

- packageを作成してからテストまで
- pip -e の代替は？
- packageの公開方法

## python 再帰的な関数を手軽に可視化する方法

- ast辿るようなものがてがるかも？
- https://gist.github.com/podhmo/6c57644f8615d8b10968a134590f5253

## line profilerを良い感じに使う

## pydantic のことに詳しくなる

### ふつうに実行する方法は?

- BaseModelを継承して定義してあれこれするのがふつうっぽい

### dictからloadする方法は？

- BaseModel.construct()? -> 違いそう
- parse_obj っぽい

しかしやっていることはdictでなければdictにして `Model(**obj)` しているだけだった。

### schemaを知る方法？

```
Person.schema()
```

### 再帰的に見る必要ある？

わかんない。とりあえず再帰的なschemaを作りたい場合には `update_forward_refs()` が必要そう。
ふつうに使う分にはpydanticのdict()を呼ぶだけで大丈夫なように観える？
あー、encoderを実行したいがために呼び直しているのか。

### `Model.dict()` の意味は？

- modelをdictに変換するもののように観える。


### Optionalの指定は

- `a : t.Optional[A]` ではなさそう
- `a : A = None` っぽい？

## python hasattrに変更した時に得られる速度

```
    64     39021      38785.0      1.0      5.0      if isinstance(obj, Enum):
    65                                                   return obj.value
```

```
    64     39021      36080.0      0.9      5.1      if hasattr(obj, "value"):  # Enum
    65                                                   return obj.value
```
