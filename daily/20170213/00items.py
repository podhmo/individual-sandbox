from marshmallow import Schema, fields
from swagger_marshmallow_codegen.validate import ItemsRange, Unique


class A(Schema):
    nums = fields.List(fields.Integer(), validate=[ItemsRange(min=1, max=10), Unique()])


class B(Schema):
    nums = fields.List(fields.Integer(validate=[ItemsRange(min=1, max=10), Unique()]))


print(A().load({"nums": ["1", "2"]}))
# print(B().load({"nums": ["1", "2"]}))
