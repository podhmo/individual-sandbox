# pymongoでcollections.UserListが利用できない

## はじめに

以下の様なエラーメッセージが出る

```
bson.errors.InvalidDocument: Cannot encode object: [{'name': 'foo', 'message': 'congrats'}]
```

## 原因

- `collections.UserDict` はok。 `collections.abc.Mapping` を見ているため。
- `collections.UserList` はng。 `collections.abc.Sequence` 用の定義が含まれていないため。

## ENCODERSの設定を追加

以下のようにすると利用するencoderを追加できる


```python
import bson
import collections


bson._ENCODERS[collections.Sequence] = bson._encode_list
```

実はこれだけだとダメで通常インストールした場合にはC拡張の方が使われるので上のhookの定義を見ない。

具体的には、今の状況で `collections.UserList` をpymongoで使えるようにするには以下の様にならないとだめ。

- `pymongo.message` の `_use_c` がFalse
- `bson` の `_USE_C` がFalse

つらい。
まじめにCで動くようにしてPR投げないとだめ。かなしい。
