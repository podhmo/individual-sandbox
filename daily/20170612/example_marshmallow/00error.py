from marshmallow import Schema, fields


class ISchema(Schema):
    n = fields.Integer()


try:
    ISchema(strict=True).load({"n": "foo"})
except Exception as e:
    print(vars(e))

# {
#   'kwargs': {},
#   'field_names': ['n'],
#   'fields': [
#   <fields.Integer(default=<marshmallow.missing>,
#     attribute=None,
#     validate=None,
#     required=False,
#     load_only=False,
#     dump_only=False,
#     missing=<marshmallow.missing>,
#     allow_none=False,
#     error_messages={
#       'invalid': 'Not a valid integer.',
#       'type': 'Invalid input type.',
#       'required': 'Missing data for required field.',
#       'validator_failed': 'Invalid value.',
#       'null': 'Field may not be null.'
#     })>],
#  'data': {'n': 'foo'},
#  'messages': {'n': ['Not a valid integer.']}
# }
