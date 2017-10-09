from marshmallow.schema import SchemaMeta, SchemaOpts


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
