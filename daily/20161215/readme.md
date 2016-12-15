# python mock objectに対するmockもやっぱりpatch経由のほうが見やすい気がした

objectのメソッドに対するmockの話。

```python
class Foo(object):
    def hello(self, v):
        raise Exception("foo")
```

に対して、objectに対してmockをかける時には以下のように書ける

```python
foo = Foo()
foo.hello = mock.Mock()
foo.hello.return_value = "yup"

assert foo.hello("bar") == "yup"
foo.hello.assert_called_once_with("bar")
```

ただ、patch経由でかけたほうが見やすい気がする。

```python
foo = Foo()
with mock.patch.object(foo, "hello") as hello:
    hello.return_value = "yup"

    assert foo.hello("bar") == "yup"
    hello.assert_called_once_with("bar")
```

# python marshmallow

以下のことをまとめておきたい感じ

- 余分な値が消えることをメモ
- requiredにしたい場合のメモ
- 雑な方法(fields)
- 速度を気にしたい場合
- load/dump時のvalidationでエラーのときにエラーにしたい場合

## 余分な値が消える

```python
from marshmallow import Schema


class Person(Schema):
    class Meta:
        fields = ("name", "age")


input_data = {"name": "foo", "age": 10, "skills": ["a", "b", "c"]}
Person().load(input_data)
# UnmarshalResult(data={'age': 10, 'name': 'foo'}, errors={})
Person().dump(input_data)
# MarshalResult(data={'age': 10, 'name': 'foo'}, errors={})
```

## 速度を気にしたい場合

```python
Person().dump(input_data, update_fields=False)
```

## load/dump時のvalidationでエラーのときにエラーにしたい場合

```python
Person(strict=True).load({"name": "foo"})
```

`strict=True` を付ける

```python
class Person(Schema):
    name = fields.String(required=True)
    age = fields.Int(required=True)


person = Person(strict=True).load(input_data)

# もしくは
class Person2(Person):
    class Meta:
        strict = True

person = Person2().load(input_data)
```

## requiredにしたい場合

fieldsなどはdefaultではrequiredにならない。

```python
# これでは欠損していてもエラーにならない
class Foo(Schema):
    class Meta:
        fields ("a","b","c")
        strict = True


# こうしないとだめ
class Foo(Schema):
    a = fields.String(required=True)
    b = fields.String(required=True)
    c = fields.String(required=True)
```
