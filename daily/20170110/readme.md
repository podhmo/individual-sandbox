# python marshmallow classを生成したあとにprocessorを追加したい

```python
def marshmallow_add_processor(cls, tag, pass_many=False, pass_original=False):
    def wrapped(fn):
        name = fn.__name__
        fn = tag_processor(tag, fn, pass_many, pass_original=pass_original)
        setattr(cls, name, fn)
        cls.__processors__[(tag, pass_many)].append(name)
        return fn
    return wrapped
```

こんな感じで使える

```python
class Person2(Schema):
    firstname = fields.String(required=True)
    lastname = fields.String(required=True)


@marshmallow_add_processor(Person2, POST_DUMP)
def fill_fullname(self, ob):
    ob["fullname"] = "{} {}".format(ob["firstname"], ob["lastname"])
```

