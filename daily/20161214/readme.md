# python enumのこと

```python
import enum


@enum.unique
class RGB(enum.Enum):
    r = "R"
    g = "G"
    b = "B"

    def foo():
        print()

if __name__ == "__main__":
    print(list(RGB))
    # [<RGB.r: 'R'>, <RGB.g: 'G'>, <RGB.b: 'B'>]
    print([e.value for e in RGB.__members__.values()])
    # ['R', 'G', 'B']

    print("R" in RGB)
    # False

    print(RGB.r in RGB)
    # True

    print(RGB("R"))
    # RGB.r

    try:
        print(RGB("X"))
    except ValueError as e:
        print(e.__class__, e)
    # <class 'ValueError'> 'X' is not a valid RGB
```

# flask python ひどいライブラリと付き合う方法

appを引数にとってオブジェクトを作る何かについて

- appをwrapしたものならappとして扱えるので問題ない
- 都度生成して良いものならthread localにしても
- そうじゃない場合には、何か色々頑張る

[example_flask](example_flask)

# marshmallow python サポートしているfieldsの一覧が欲しい

直接 fields.py を覗くのが手軽。

```python
__all__ = [
    'Field',
    'Raw',
    'Nested',
    'Dict',
    'List',
    'String',
    'UUID',
    'Number',
    'Integer',
    'Decimal',
    'Boolean',
    'FormattedString',
    'Float',
    'DateTime',
    'LocalDateTime',
    'Time',
    'Date',
    'TimeDelta',
    'Url',
    'URL',
    'Email',
    'Method',
    'Function',
    'Str',
    'Bool',
    'Int',
    'Constant',
]
```

# marshmallow python defaultの引数に関数を指定する方法

defaultの引数に関数を指定する方法。普通に渡せば良い

```python
from marshmallow import Schema, fields
from datetime import datetime


class Foo(Schema):
    created_at = fields.DateTime(default=datetime.now)

print("load:", Foo().load({}).data)
print("dump:", Foo().dump({}).data)
```

```
load: {}
dump: {'created_at': datetime.datetime(2016, 12, 14, 12, 58, 10, 789947)}
```

以下のような記述がある

```python
class Field(FieldABC):
    def serialize(self, attr, obj, accessor=None):
        """Pulls the value for the given key from the object, applies the
        field's formatting and returns the result.

        :param str attr: The attibute or key to get from the object.
        :param str obj: The object to pull the key from.
        :param callable accessor: Function used to pull values from ``obj``.
        :raise ValidationError: In case of formatting problem
        """
        if self._CHECK_ATTRIBUTE:
            value = self.get_value(attr, obj, accessor=accessor)
            if value is missing_:
                if hasattr(self, 'default'):
                    if callable(self.default):
                        return self.default()
                    else:
                        return self.default
        else:
            value = None
        return self._serialize(value, attr, obj)
```
