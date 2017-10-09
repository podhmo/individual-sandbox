from marshmallow import Schema, fields, validates_schema, ValidationError


class UserSchema(Schema):
    first_attr = fields.Field(required=False)
    second_attr = fields.Field(required=False)

    @validates_schema
    def which(self, data):
        if len([x for x in (data.get("first_attr"), data.get("second_attr")) if x]) != 1:
            raise ValidationError("which first_attr or second_attr")


print(UserSchema().load({}))
print(UserSchema().load({"first_attr": 1}))
print(UserSchema().load({"second_attr": 2}))
print(UserSchema().load({"first_attr": 1, "second_attr": 2}))
