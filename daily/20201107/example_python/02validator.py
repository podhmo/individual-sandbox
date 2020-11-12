import typing as t


class InvalidField(Exception):
    def __init__(self, msg, *, name, value, original=None) -> None:
        super().__init__(msg)
        self.name = name
        self.value = value
        self.original = original


class ValidationError(Exception):
    pass


class Field:
    def __init__(self, default_value) -> None:
        self.name = None
        self.default_value = default_value

    def __set_name__(self, cls, name) -> None:
        try:
            cls._validators[name] = self
        except AttributeError:
            cls._validators = {}
            return self.__set_name__(cls, name)

        self.name = name


class Int(Field):
    def validate(self, ob, name, value) -> t.Any:
        if value is None:
            value = self.default_value
        try:
            return int(value)
        except TypeError as e:
            raise InvalidField(str(e), name=name, value=value, original=e)


def validate(schema, d, *, naive=False) -> dict:
    errors = {}
    for name, v in (getattr(schema, "_validators", None) or {}).items():
        try:
            d[name] = v.validate(schema, name, d.get(name))
        except InvalidField as e:
            errors[name] = e
        except Exception as e:
            if naive:
                raise
            errors[name] = InvalidField(
                str(e), name=name, value=d.get(name), original=e
            )
    if errors:
        raise ValidationError(errors)
    return d


class S:
    name: str
    age: int = Int(0)


print(validate(S, {"name": "foo", "age": 20}))
print(validate(S, {"name": "foo", "age": "foo"}))
