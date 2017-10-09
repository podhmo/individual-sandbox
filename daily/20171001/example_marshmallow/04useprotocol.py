from marshmallow.schema import BaseSchema
from marshmallow import fields, validate
from marshmallow_protocol import CustomMeta, ProtocolError


def x_vendor_prefix_only_metadata(cls):
    for f in cls._declared_fields.values():
        for k in f.metadata.keys():
            if not k.startswith("x_"):
                raise ProtocolError(k)


class Schema(BaseSchema, metaclass=CustomMeta):
    class Meta:
        protocol = x_vendor_prefix_only_metadata


class S(Schema):
    # marshmallow_protocol.ProtocolError: validator
    v = fields.Integer(validator=validate.Range(max=10))


class S(Schema):
    v = fields.Integer(validate=validate.Range(max=10), x_ja="値")


class Ignored(Schema):
    class Meta:
        protocol = None

    v = fields.Integer(validator=validate.Range(max=10))


Ignored().load({"v": 1000000000})
