from marshmallow import fields, Schema


class Value(Schema):
    x_point = fields.Integer(dump_to="xPoint", load_from="xPoint")
    y_point = fields.Integer(dump_to="yPoint", load_from="yPoint")
    xy = fields.Method("get_xy")

    def get_xy(self, ob):
        return ob["x_point"] / ob["y_point"]


data = Value().load({"xPoint": 10, "yPoint": 100}).data
print(data)
print(Value().dump(data))
