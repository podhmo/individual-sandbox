from marshmallow.schema import BaseSchema, SchemaMeta
from marshmallow import fields
"""
class Field(FieldABC):
    def __init__(self, default=missing_, attribute=None, load_from=None, dump_to=None,
                 error=None, validate=None, required=False, allow_none=None, load_only=False,
                 dump_only=False, missing=missing_, error_messages=None, **metadata):
        # .. snip
        self.metadata = metadata
"""


class MySchemaMeta(SchemaMeta):
    def __init__(self, name, bases, attrs):
        super().__init__(name, bases, attrs)
        for k, f in self._declared_fields.items():
            print(k, f.metadata)


class Schema(BaseSchema, metaclass=MySchemaMeta):
    pass


class S(Schema):
    v = fields.Integer(x_label="値")


print(S())
