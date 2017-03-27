from marshmallow import Schema, fields
from marshmallow import pre_load, post_load


class Inner(Schema):
    a = fields.String(required=True, default="a")
    b = fields.String(required=True, default="b")
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

    @pre_load(pass_many=True)
    def store_priority(self, data, many):
        if many:
            self.context["priority"] = [d.get("priority") for d in data]
        else:
            self.context["priority"] = data.get("priority")
        return data

    @post_load(pass_many=True)
    def unflatten_priority(self, data, many):
        if many:
            for i, d in enumerate(data):
                if self.context["priority"][i] is not None:
                    d["d"]["priority"] = self.context["priority"][i]
        else:
            if self.context.get("priority") is not None:
                data["d"]["priority"] = self.context["priority"]
        return data

print("S")
print(S().load({}))
print(S().load({"d": {"priority": 2}}), "hmm")
print(S().load({"priority": 2}))

print("S one")
s = S()
print(S().load({}))
print(S().load({"priority": 2}))
print(S().load({}))

print("S many")
s = S(many=True)
print(s.load([{}, {}, {}]))
print(s.load([{}, {"priority": 2}, {}]))
