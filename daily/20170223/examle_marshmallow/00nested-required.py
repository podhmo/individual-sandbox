from marshmallow import Schema, fields


class Data(Schema):
    contents = fields.List(fields.Nested('Content'))


class Content(Schema):
    content = fields.Nested('Item', required=True)


class Item(Schema):
    key = fields.String()


d = {"contents": [{"throughContent": {"key": "hmm"}}]}
print(Data().load(d))
