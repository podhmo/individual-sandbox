from marshmallow.validate import Regexp
from marshmallow import Schema, fields
from marshmallow import pre_load, post_load, pre_dump, post_dump
from marshmallow import UnmarshalResult, MarshalResult


# hmm.

class PrimitiveSchema(Schema):
    v = None
    default = {}

    @pre_load
    def to_dict(self, v):
        return {"v": v}

    @post_load
    def to_v(self, v):
        return v.get("v")

    @pre_dump
    def to_dict2(self, v):
        return {"v": v}

    @post_dump
    def to_v2(self, v):
        return v.get("v")

    def load(self, data, many=None, partial=None):
        r = super().load(data, many=many, partial=partial)
        if r.errors:
            if self.many if many is None else bool(many):
                # xxx:
                data = [d.get("v") or self.default for d in r.data]
                errors = {k: v["v"] for k, v in r.errors.items()}
            else:
                data = data["v"]
                errors = r.errors["v"]
            return UnmarshalResult(data=data, errors=errors)
        return r

    def dump(self, obj, many=None, update_fields=True, **kwargs):
        r = super().dump(obj, many=many, update_fields=update_fields, **kwargs)
        if r.errors:
            if self.many if many is None else bool(many):
                # xxx:
                data = [d.get("v") or self.default for d in r.data]
                errors = {k: v["v"] for k, v in r.errors.items()}
            else:
                data = data["v"]
                errors = r.errors["v"]
            return MarshalResult(data=data, errors=errors)
        return r


class ValueSchema(PrimitiveSchema):
    v = fields.String(validate=[Regexp(".+@.+")])

emails = ["a@b.jp", "x@gmail.com"]
print(ValueSchema(many=True).load(emails))
emails = ["a@b.jp", "x@gmail.com", "xy"]
print(ValueSchema(many=True).load(emails))


class DateSchema(PrimitiveSchema):
    v = fields.Date()

ds = ["1"]
print(DateSchema(many=True).dump(ds))
ds = ["1", "2", "3"]
print(DateSchema(many=True).dump(DateSchema(many=True).load(ds).data))
