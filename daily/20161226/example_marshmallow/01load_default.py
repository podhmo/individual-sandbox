from marshmallow import fields, Schema


class S(Schema):
    name = fields.String(missing="foo")

if __name__ == "__main__":
    d0 = {}
    print("input", d0)
    d1, err = S().load(d0)
    print("loaded", d1)
    d2, err = S().dump(d1)
    print("dumped", d2)
