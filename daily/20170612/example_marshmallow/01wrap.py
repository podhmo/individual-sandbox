from marshmallow import (
    fields,
)
from swagger_marshmallow_codegen.schema import PrimitiveValueSchema
import re
from marshmallow.validate import Regexp


class S(PrimitiveValueSchema):
    v = fields.String(validate=[Regexp(regex=re.compile('.+@.+'))])


email_list = [
    "foo@gmail.com",
    "bar@gmail.com",
    "boo@gmail.com",
    "bad-email",
]

print(S(many=True).load(email_list))
