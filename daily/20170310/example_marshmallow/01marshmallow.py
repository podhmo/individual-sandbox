from marshmallow import Schema, fields
from collections import ChainMap


class Person(Schema):
    name = fields.String(required=True)
    age = fields.Integer(required=True)
    father = fields.Nested('self')
    mother = fields.Nested('self')


class Person2(Person):
    def _do_load(self, data, many=None, partial=None, postprocess=True):
        result, errors = super()._do_load(data, many=many, partial=partial, postprocess=postprocess)
        if not many:
            return ChainMap(result, data), errors
        else:
            return [ChainMap(x, y) for x, y in zip(result, data)], errors


data = {"name": "foo", "age": 10, "memo": "hai"}
print(Person().load(data))
# UnmarshalResult(data={'name': 'foo', 'age': 10}, errors={})
print(Person2().load(data))
# UnmarshalResult(data=ChainMap({'name': 'foo', 'age': 10}, {'name': 'foo', 'age': 10, 'memo': 'hai'}), errors={})

data = [{"name": "foo", "age": 10, "memo": "hai"}, {"name": "bar", "age": 20, "memo": "hai"}]
print(Person2().load(data, many=True))
# UnmarshalResult(data=[ChainMap({'name': 'foo', 'age': 10}, {'name': 'foo', 'memo': 'hai', 'age': 10}), ChainMap({'name': 'bar', 'age': 20}, {'name': 'bar', 'memo': 'hai', 'age': 20})], errors={})
