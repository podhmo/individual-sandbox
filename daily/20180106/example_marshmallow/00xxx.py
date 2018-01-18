from marshmallow import Schema, fields, validate


class S(Schema):
    facebook_profile_url = fields.String(
        required=False,
        validate=validate.Regexp(
            '^$|https://facebook.com/[a-zA-Z][a-zA-Z 0-9._]{0,50}$', 0,
            'Facebook username is invalid'
        )
    )


print(S().load({}))
print(S().load({"facebook_profile_url": ""}))
print(S().load({"facebook_profile_url": "https://facebook.com/a"}))
