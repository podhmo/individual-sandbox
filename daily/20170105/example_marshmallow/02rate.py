from marshmallow import Schema, fields, post_load


class Data(Schema):
    sessions = fields.Integer(missing=0)
    conversions = fields.Integer(missing=0)
    conversionRate = fields.Float()

    @post_load
    def fill_rate(self, data):
        if "conversionRate" in data:
            return data

        if data["sessions"] == 0:
            data["conversionRate"] = 0.0
        else:
            data["conversionRate"] = float(data["conversions"]) / data["sessions"]
        return data

print(Data().load({}))
# UnmarshalResult(data={'conversions': 0, 'sessions': 0, 'conversionRate': 0.0}, errors={})

print(Data().load({"sessions": "1000", "conversions": "10"}))
# UnmarshalResult(data={'conversions': 10, 'conversionRate': 0.01, 'sessions': 1000}, errors={})

print(Data().load({"sessions": "1000", "conversions": "10", "conversionRate": "9"}))
# UnmarshalResult(data={'conversions': 10, 'sessions': 1000, 'conversionRate': '9'}, errors={})

