from marshmallow import Schema, fields, pre_load, post_load


class UserSchema(Schema):
    name = fields.Str()
    slug = fields.Str()

    @pre_load
    def slugify_name(self, in_data):
        in_data['slug'] = in_data['slug'].lower().strip().replace(' ', '-')
        return in_data


try:
    print(UserSchema().load({'name': 'Steve'}))
except KeyError as e:
    print(e)
else:
    raise Exception("not raised")


class UserSchema2(Schema):
    name = fields.Str()
    slug = fields.Str(missing="")

    @post_load
    def slugify_name(self, in_data):
        in_data['slug'] = in_data['slug'].lower().strip().replace(' ', '-')
        return in_data


print(UserSchema2().load({'name': 'Steve'}))
print(UserSchema2().load({'name': 'Steve', "slug": "Foo Bar Boo"}))
