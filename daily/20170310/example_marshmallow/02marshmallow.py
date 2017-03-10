from marshmallow import Schema, fields


class M(object):  # todo: to mapping
    def __init__(self, data, fields, default=fields.Field()):
        self.data = data
        self.fields = fields
        self.default = default

    def items(self):
        return ((k, self[k]) for k in self.data)

    def __getattr__(self, name):
        return getattr(self.fields, name)

    def __getitem__(self, name):
        return self.fields.get(name) or self.default


class UWrap(object):  # todo: Wrap for serialize
    def __init__(self, unmarshal):
        self.unmarshal = unmarshal

    def __getattr__(self, name):
        return getattr(self.unmarshal, name)

    def __call__(self, data, fields_dict, *args, **kwargs):
        return self.unmarshal.deserialize(data, M(data, fields_dict), *args, **kwargs)


class Person(Schema):
    name = fields.String(required=True)
    age = fields.Integer(required=True)
    father = fields.Nested('self')
    mother = fields.Nested('self')


class Person2(Person):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._unmarshal = UWrap(self._unmarshal)

data = {"name": "foo", "age": 10, "memo": "hai"}
print(Person().load(data))
# UnmarshalResult(data={'name': 'foo', 'age': 10}, errors={})
print(Person2().load(data))
# UnmarshalResult(data={'age': 10, 'memo': 'hai', 'name': 'foo'}, errors={})
