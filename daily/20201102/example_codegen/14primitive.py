from marshmallow import Schema, fields


class PrimitiveSchema(Schema):
    id = fields.String()
    name = fields.String()


class FactorySchema(Schema):
    widget_ids = fields.List(fields.Pluck(WidgetSchema, "id"))


print(FactorySchema().load({"widget_ids": ["a", "b"]}))

