from marshmallow.schema import BaseSchema, SchemaMeta, SchemaOpts
from marshmallow import fields
from marshmallow import validate


class CustomOpts(SchemaOpts):
    protocol = None


class CustomMeta(SchemaMeta):
    def __init__(self, name, bases, attrs):
        super().__init__(name, bases, attrs)
        k = "_protocol_verified"
        protocol = self.Meta.protocol
        if protocol is not None and k not in self.__dict__:
            protocol(self)
            setattr(self, k, protocol)


class ProtocolError(Exception):
    pass


def x_vendor_prefix_only_metadata(cls):
    for f in cls._declared_fields.values():
        for k in f.metadata.keys():
            if not k.startswith("x_"):
                raise ProtocolError(k)


class Schema(BaseSchema, metaclass=CustomMeta):
    class Meta:
        protocol = x_vendor_prefix_only_metadata


class S0(Schema):
    class Meta:
        protocol = None

    v = fields.Integer(ja=validate.Range(max=10))


class S(Schema):
    v = fields.Integer(ja=validate.Range(max=10))
    # ProtocolError: ja
