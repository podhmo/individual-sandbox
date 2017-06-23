from marshmallow import Schema, fields
from marshmallow import post_load, pre_load


class S(Schema):
    x = fields.Number()
    n = fields.Number()
    rate = fields.Number()

    @pre_load
    def fill_rate(self, data):
        if data.get("rate") == None:
            if data["n"] == 0:
                data["rate"] = 0
            else:
                data["rate"] = data["x"] / data["n"]
        return data


print(S(strict=True).load({"x": 10, "n": 100, "rate": None}))
print(S(strict=True).load({"x": 0, "n": 0, "rate": None}))
print(S(strict=True).load({"x": 0, "n": 0}))
