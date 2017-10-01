from marshmallow.schema import BaseSchema, SchemaMeta
from marshmallow import fields
from marshmallow import validate


class CustomMeta(SchemaMeta):
    def __new__(self, name, bases, attrs, protocol=None):
        return super().__new__(self, name, bases, attrs)

    def __init__(self, name, bases, attrs, protocol=None):
        super().__init__(name, bases, attrs)
        k = "_protocol_verified"
        if protocol is not None:
            self.protocol = protocol
        else:
            protocol = self.protocol

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


class Schema(BaseSchema, metaclass=CustomMeta, protocol=x_vendor_prefix_only_metadata):
    pass


class S(Schema):
    v = fields.Integer(ja=validate.Range(max=10))
    # ProtocolError: ja
