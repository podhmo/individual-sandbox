from marshmallow import Schema, fields


class Product(Schema):
    product_id = fields.String()
    description = fields.String(description="Description of product.")


class ProductList(Schema):
    products = fields.List(fields.Nested(Product))
