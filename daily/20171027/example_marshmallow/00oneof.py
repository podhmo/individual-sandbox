# https://github.com/marshmallow-code/marshmallow/issues/693
from marshmallow import Schema, fields
from swagger_marshmallow_codegen.schema import OneOfSchema


class Value(Schema):
    x = fields.String(required=True)
    y = fields.String(required=True)


class S(OneOfSchema):
    schema_classes = (Value, "self")

    op = fields.String(required=True)
    field = fields.Nested("self", required=True)


# atom
# one
# both

# many
# serialize
d = {"x": "bar", "y": "baz"}
print("ok", S().load(d))
d = {"x": object(), "y": "baz"}
print("ng", S().load(d))
d = {"op": "val", "field": {"x": "bar", "y": "baz"}}
print("ok", S().load(d))
d = {"op": "val", "field": {"x": "bar", "y": "baz"}, "x": "bar", "y": "baz"}
print("ng", S().load(d))
d = {"op": "val", "field": {"x": "bar", "y": "baz"}, "x": "bar"}
print("ok", S().load(d))
d = {"op": "val", "field": {"x": object(), "y": "baz"}, "x": "bar"}
print("ng", S().load(d))
d = {'op': 'foo', 'field': {'op': 'val', 'field': {'x': 'bar', 'y': 'baz'}}}
print("ok", S().load(d))

print("----------------------------------------")

d = [{"x": "bar", "y": "baz"}]
print("ok", S(many=True).load(d))
d = [{"op": "val", "field": {"x": "bar", "y": "baz"}}]
print("ok", S(many=True).load(d))
d = [{"op": "val", "field": {"x": "bar", "y": "baz"}, "x": "bar", "y": "baz"}]
print("ng", S(many=True).load(d))
d = [{"op": "val", "field": {"x": "bar", "y": "baz"}, "x": "bar"}]
print("ok", S(many=True).load(d))
d = [{'op': 'foo', 'field': {'op': 'val', 'field': {'x': 'bar', 'y': 'baz'}}}]
print("ok", S(many=True).load(d))

print("----------------------------------------")
d = {"x": "bar", "y": "baz"}
print("ok", S().dump(d))
d = {"x": object(), "y": "baz"}
print("ok", S().dump(d))
d = {"op": "val", "field": {"x": "bar", "y": "baz"}}
print("ok", S().dump(d))
d = {"op": "val", "field": {"x": "bar", "y": "baz"}, "x": "bar", "y": "baz"}
print("ng", S().dump(d))
d = {"op": "val", "field": {"x": "bar", "y": "baz"}, "x": "bar"}
print("ok", S().dump(d))
d = {"op": "val", "field": {"x": object(), "y": "baz"}, "x": "bar"}
print("ng", S().dump(d))
d = {'op': 'foo', 'field': {'op': 'val', 'field': {'x': 'bar', 'y': 'baz'}}}
print("ok", S().dump(d))
