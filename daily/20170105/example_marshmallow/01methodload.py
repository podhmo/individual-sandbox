from marshmallow import Schema, fields, post_load


class X(Schema):
    x = fields.Float(required=True)
    y = fields.Float(required=True)
    xy = fields.Float()

    @post_load
    def load_xy(self, ob):
        if "xy" not in ob:
            ob["xy"] = ob["x"] * ob["y"]
        return ob

print(X().load({"x": 2.0, "y": 3.0}))
