# hmmmmmmmmmmmmmmmm
import json
from marshmallow.validate import Regexp
from marshmallow import Schema, fields, pre_load, post_load
from marshmallow import MarshalResult, UnmarshalResult
from marshmallow import utils


# hmm.
# class PrimitiveValueSchema(object):
#     field = None

#     class opts:
#         json_module = json

#     def __init__(self, many=False, strict=False):
#         self.many = many
#         self.strict = strict

#     def __repr__(self):
#         return '<{ClassName}(many={self.many}, strict={self.strict})>'.format(
#             ClassName=self.__class__.__name__, self=self
#         )

#     def dump(self, obj, many=None, update_fields=True, **kwargs):
#         many = self.many if many is None else bool(many)
#         if many and utils.is_iterable_but_not_string(obj):
#             obj = list(obj)
#         # todo: processor
#         if many or self.many:
#             return []
#         return []

#     def dumps(self, obj, many=None, update_fields=True, *args, **kwargs):
#         deserialized, errors = self.dump(obj, many=many, update_fields=update_fields)
#         ret = self.opts.json_module.dumps(deserialized, *args, **kwargs)
#         return MarshalResult(ret, errors)

#     def load(self, data, many=None, partial=None):
#         many = self.many if many is None else bool(many)
#         if many and utils.is_iterable_but_not_string(data):
#             data = list(data)
#         if many:
#             r = []
#             for i, d in enumerate(data):
#                 try:
#                     r.append(self.field.deserialize(d))
#                 except validate.ValidationError as e:
#                     errors[i] = e
#             return r
#         else:
#             return self.field.deserialize(data)

#     def loads(self, json_data, many=None, *args, **kwargs):
#         partial = kwargs.pop('partial', None)
#         data = self.opts.json_module.loads(json_data, *args, **kwargs)
#         return self.load(data, many=many, partial=partial)


# class ValueSchema(PrimitiveValueSchema):
#     field = fields.String(validate=[Regexp(".+@.+")])
from marshmallow.marshalling import Unmarshaller

um = Unmarshaller()
field = fields.String(validate=[Regexp(".+@.+")])
um.deserialize("xy", field)




# print(field.serialize(None, "xy", accessor=lambda ck, ob, default: ob))

# emails = ["a@b.jp", "x@gmail.com", "xy"]
# print(ValueSchema(many=True).load(emails))
# print(ValueSchema(many=False).load("xs"))
