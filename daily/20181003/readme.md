## mypy pluginsの有効化

mypy.iniでとかやる？

```
[mypy]
plugins = foo/plugin.py
```

https://github.com/python/mypy/issues/1240

実際のコードはこの辺

https://github.com/python/mypy/blob/b37d08b5d48bcbb8a2461d36541099f4b135a08b/mypy/build.py#L307

mypy.iniの書き方

https://mypy.readthedocs.io/en/latest/config_file.html

## mypy check

再帰的に調べるなら `mypy -p <package>` が楽そう

```
Running code:
  Specify the code you want to type check. For more details, see
  mypy.readthedocs.io/en/latest/running_mypy.html#running-mypy

  -m MODULE, --module MODULE
                            Type-check module; can repeat for more modules
  -p PACKAGE, --package PACKAGE
                            Type-check package recursively; can be repeated
  -c PROGRAM_TEXT, --command PROGRAM_TEXT
                            Type-check program passed in as string
  files                     Type-check given files or directories
```

## mypy typed row

- https://github.com/python/typing/issues/28

pandas.DataFrameなどはどうやって型をつける？ > pluginで

### pluginの定義の仕方


- https://github.com/python/mypy/pull/3517
- https://github.com/python/mypy/issues/5409
- https://github.com/TypedDjango/django-stubs/issues/5

すごく単純には以下の様な形。Pluginを継承してそのhookを差し込む。(mypyの[test-data](https://github.com/python/mypy/tree/master/test-data)を見るのが一番便利)

```python
from mypy.plugin import Plugin

class MyPlugin(Plugin):
    def get_function_hook(self, fullname):
        if fullname == '__main__.f':
            return my_hook
        assert fullname is not None
        return None

def my_hook(ctx):
    return ctx.api.named_generic_type('builtins.int', [])

def register(version):
    return MyPlugin
```

あとは安直にmypy/plugin.pyの中を除くのが良い。[open-callback](https://github.com/python/mypy/blob/f59a8713df613a331202e4f02fbbc418a6f99628/mypy/plugin.py#L386)とか

直接コードを読む方法も

https://github.com/python/mypy/blob/master/mypy/plugins/dataclasses.py

使うもの

- mypy.plugin.ClassDefContext

```
from mypy.types import (
    CallableType, Instance, NoneTyp, Overloaded, TypeVarDef, TypeVarType,
)
```

- dataclass_makers
- DataclassAttribute
- DataclassTransformer
- dataclass_class_maker_callback
- 

## mypy descriptor

状況を整理したい

## python sqliteの力を

../20180208/example_sql/insert.sql

やってみたいことは２つ

- data loader的な仕組みを手軽に
- ちょっとだけstrictなquery

### hmm

- https://docs.python.jp/3/library/sqlite3.html
- https://docs.python.jp/3/library/sqlite3.html#sqlite3-types
- https://docs.python.jp/3/library/sqlite3.html#using-shortcut-methods
- https://www.craneto.co.jp/archives/1248/
- https://qiita.com/mas9612/items/a881e9f14d20ee1c0703

CRUD
prepared statement
register,adapter
他のdbとの共通部分
query log
Row

### query log

```python
with sqlite.connect(...) as conn:
    conn.set_trace_callback(print)
```

### adapter,converter

- adater -- `<Python object>` -> `<Primitive value>`
- converter -- `<Primitive value>` -> `<Python object>`

```python
def register_adapters_and_converters():
    def adapt_date(val):
        return val.isoformat()

    def convert_date(val):
        return datetime.date(*map(int, val.split(b"-")))
```

### prepared statement

two styles are existed

- qmark style
- named style

```python
import sqlite3

con = sqlite3.connect(":memory:")
cur = con.cursor()
cur.execute("create table people (name_last, age)")

who = "Yeltsin"
age = 72

# This is the qmark style:
cur.execute("insert into people values (?, ?)", (who, age))

# And this is the named style:
cur.execute("select * from people where name_last=:who and age=:age", {"who": who, "age": age})

print(cur.fetchone())
```

### Row

```python
conn = sqlite3.connect(":memory:")
c = conn.cursor()
c.execute('''create table stocks
(date text, trans text, symbol text,
 qty real, price real)''')
c.execute("""insert into stocks
          values ('2006-01-05','BUY','RHAT',100,35.14)""")
conn.commit()
c.close()
```

## marshmallow+oneOf


## discord.py ちょっと中覗いてみる

- https://github.com/Rapptz/discord.py
- https://discordpy.readthedocs.io/en/latest/
