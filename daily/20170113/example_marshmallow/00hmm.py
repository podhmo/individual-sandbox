from marshmallow import Schema, fields, pre_load


class S(Schema):
    @pre_load
    def when_none(self, d):
        d.get("vs") is None
        d["vs"] = []
        return d
    vs = fields.List(fields.String(), allow_none=True)

print(S().load({"vs": None}))
# UnmarshalResult(data={'vs': None}, errors={}) heh
