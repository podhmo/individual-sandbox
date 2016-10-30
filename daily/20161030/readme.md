# python pickle 薄いdb

pickle使った薄いdb。sqliteすら使いたくない場合のやつ。
よく考えたらpickleである必要もないかもしれない。

- [example_tinydb](./example_tinydb)

# python pyjwtの使い方

```python
import jwt

data = {"some": "payload"}
token = jwt.encode(data, 'secret', algorithm="HS256")
data2 = jwt.decode(token, "secret")
```

```
raw: {'some': 'payload'}
encoded: b'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzb21lIjoicGF5bG9hZCJ9.4twFt5NiznN84AWoo1d7KO1T_yoc0Z6XOpOVswacPZg'
decoded: {'some': 'payload'}
```

# python simpleなuser auth

passwordをhash化して保持しておこうという話。
salt付きでhash化するのは当然なのだけれど。stretchもある程度やっておいた方が良いらしい。

- [example_auth](./example_auth)

# python bottle proxy2 oauth auth

たぶん python-social-authが一番楽そうな感じ？

- [omab/python-social-auth: Social auth made simple](https://github.com/omab/python-social-auth)

## python-social-authで個人的なbackendsを作成する方法

以下を実装する必要がありそう。真面目にやる場合には。通常の用途では既に定義済みなのでこれを使えば良い。

- strategy
- backend
