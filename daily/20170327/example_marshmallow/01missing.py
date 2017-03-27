from marshmallow import Schema, fields


class Inner(Schema):
    a = fields.String()
    b = fields.String()
    priority = fields.Int()

    @classmethod
    def loading(cls, d):
        return cls(strict=True).load(d).data

    @classmethod
    def new(cls, priority=0):
        return cls.loading({
            "a": "a",
            "b": "b",
            "priority": priority
        })


class S(Schema):
    d = fields.Nested(Inner, missing=Inner.new)


class Inner2(Schema):
    a = fields.String(missing="a")
    b = fields.String(missing="b")
    priority = fields.Int(missing=0)


class S2(Schema):
    d = fields.Nested(Inner2)


class Inner3(Schema):
    a = fields.String(missing="a")
    b = fields.String(missing="b")
    priority = fields.Int(missing=0)

    @classmethod
    def loading(cls, d):
        return cls(strict=True).load(d).data

    @classmethod
    def new(cls, priority=0):
        return cls.loading({
            "priority": priority
        })


class S3(Schema):
    d = fields.Nested(Inner3, missing=Inner3.new)


print("S")
print(S().load({}))
print(S().load({"d": {"priority": 2}}), "hmm")
print("S2")
print(S2().load({}), "hmm")
print(S2().load({"d": {"priority": 2}}))
print("S3")
print(S3().load({}))
print(S3().load({"d": {"priority": 2}}))
