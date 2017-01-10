from marshmallow import Schema, fields, post_dump
from marshmallow.decorators import POST_DUMP, tag_processor


class Person(Schema):
    firstname = fields.String(required=True)
    lastname = fields.String(required=True)

    @post_dump
    def fill_fullname(self, ob):
        ob["fullname"] = "{} {}".format(ob["firstname"], ob["lastname"])

print(Person().dump({"firstname": "foo", "lastname": "bar"}))


class Person2(Schema):
    firstname = fields.String(required=True)
    lastname = fields.String(required=True)


def marshmallow_add_processor(cls, tag, pass_many=False, pass_original=False):
    def wrapped(fn):
        name = fn.__name__
        fn = tag_processor(tag, fn, pass_many, pass_original=pass_original)
        setattr(cls, name, fn)
        cls.__processors__[(tag, pass_many)].append(name)
        return fn
    return wrapped


@marshmallow_add_processor(Person2, POST_DUMP)
def fill_fullname(self, ob):
    ob["fullname"] = "{} {}".format(ob["firstname"], ob["lastname"])


print(Person2().dump({"firstname": "foo", "lastname": "bar"}))
