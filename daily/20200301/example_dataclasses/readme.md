# configをdataclassesで記述してみる

```console
$ make 00
python config.py
{
  "app": {
    "db": "sqlite:///:memory:"
  },
  "thirdparty": {
    "foo_token": "........................................",
    "bar_token": "........................................"
  }
}
```

```console
$ make 01
python use.py --config config.py:config
db url is sqlite:///:memory:
```

```console
$ jqfpy 'get("app/db")' <(python config.py)
"sqlite:///:memory:"
```
