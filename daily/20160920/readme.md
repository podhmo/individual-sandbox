# python state machine

- [tyarkoni/transitions: A lightweight, object-oriented finite state machine implementation in Python](https://github.com/tyarkoni/transitions)

# python logrusに近いlogger

structlogを使う？

- [Structured Logging for Python — structlog documentation](http://www.structlog.org/en/stable/)

問題: 可変のfieldを定義できない？できる？ LTSV形式で出力できない？

この辺見ると良い

- http://www.structlog.org/en/stable/standard-library.html#suggested-configuration

```
time:2016-09-20T12:40:57.29149432+09:00
level:info
msg:xxx-this-is-message
caller:<file>:<lineno>
source:increfetch

あと自由に追加できる
```

# python connexion


- [zalando/connexion: Swagger/OpenAPI First framework for Python on top of Flask with automatic endpoint validation and OAuth2 support](
https://github.com/zalando/connexion)

Open API specification (2.0)
- [OpenAPI-Specification/2.0.md at OpenAPI.next · OAI/OpenAPI-Specification]
(https://github.com/OAI/OpenAPI-Specification/blob/OpenAPI.next/versions/2.0.md#fixed-fields-7)

#### handling

利用するyamlに情報を含めたい。

- api.yamlに `{{varname}}`
- app.pyのところで `app.add_apiのところでarguments={'varname': xxxx}`

各yamlで利用するviewを指定したい

- yaml内の各pathのoperationId に `<module>.<symbol>`

custom typeの定義の仕方

- [Connexion Cookbook — Connexion 0.5 documentation](https://connexion.readthedocs.io/en/latest/cookbook.html#custom-type-format)

内部ではjsonschemaの設定を使っているっぽい。

```python
from jsonschema import draft4_format_checker

@draft4_format_checker.checks('money')
def is_money(val):
    ...
```

swaggerのyamlからapi serverを立ち上げる

```
connexion run your_api.yaml --stub --debug
```

swaggerのuiは `open http://localhost:5000/ui`

https://connexion.readthedocs.io/en/latest/cli.html


# python mongodb

- pymongoは単なるclient
- ORM的なものは色々ある。mongoengineが一番有名っぽい？
