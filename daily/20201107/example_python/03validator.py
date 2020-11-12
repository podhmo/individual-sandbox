from __future__ import annotations
import typing as t
import dataclasses

# TODO: nested
# TODO: path
# TODO: from dict


class InvalidField(Exception):
    def __init__(self, msg, *, name, value, original=None) -> None:
        super().__init__(msg)
        self.name = name
        self.value = value
        self.original = original


class ValidationError(Exception):
    pass


class Field(dataclasses.Field):
    def __init__(
        self,
        *,
        default=dataclasses.MISSING,
        default_factory=dataclasses.MISSING,
        init=True,
        repr=True,
        hash=None,
        compare=True,
        metadata=None,
    ) -> None:
        if (
            default is not dataclasses.MISSING
            and default_factory is not dataclasses.MISSING
        ):
            raise ValueError("cannot specify both default and default_factory")
        return super().__init__(
            default, default_factory, init, repr, hash, compare, metadata
        )

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
            value = self.default
        try:
            return int(value)
        except TypeError as e:
            raise InvalidField(str(e), name=name, value=value, original=e)


class MaxItems(Field):
    def validate(self, ob, name, value) -> t.Any:
        limit = self.metadata["limit"]
        if limit < len(value):
            raise InvalidField(
                f"len(ob.{name}) == {len(value)} > {limit}", name=name, value=value
            )
        return value

# nested不要では？, validateでListもDictも気にすれば良い？
class Nested(Field):
    def validate(self, ob, name, value, *, _none_type=type(None)) -> t.Any:
        try:
            typ = self.metadata["type"]
        except KeyError:
            # todo:cache
            f = getattr(ob, dataclasses._FIELDS)[name]
            typ = f.type
            if isinstance(typ, str):
                typ = t.ForwardRef(typ)._evaluate(globals(), None)

        # TODO: required check
        if value is None:
            if _none_type in t.get_args(typ):
                return value
            else:
                raise InvalidField("required", name=name, value=value)

        try:
            return validate(value)
        except ValidationError as e:
            raise InvalidField(str(e), name=name, value=value, original=e)


def validate(ob, *, naive=False) -> dict:
    params = getattr(ob.__class__, dataclasses._PARAMS)
    if params.frozen:
        return _validate_with_copy(ob, naive=naive)
    else:
        return _validate_with_update(ob, naive=naive)


def _validate_with_update(ob, *, naive=False) -> dict:
    errors = {}
    for name, v in (getattr(ob, "_validators", None) or {}).items():
        try:
            # todo: frozen
            setattr(ob, name, v.validate(ob, name, getattr(ob, name, None)))
        except InvalidField as e:
            errors[name] = e
        except Exception as e:
            if naive:
                raise
            errors[name] = InvalidField(
                str(e), name=name, value=getattr(ob, name, None), original=e
            )
    if errors:
        raise ValidationError(errors)
    return ob


def _validate_with_copy(ob, *, naive=False) -> dict:
    errors = {}
    d = {}
    _validators = getattr(ob, "_validators", None) or {}

    for f in dataclasses.fields(ob):
        name = f.name
        v = _validators.get(name)

        if v is None:
            d[name] = getattr(ob, name)
            continue

        try:
            # todo: frozen
            d[name] = v.validate(ob, name, getattr(ob, name, None))
        except InvalidField as e:
            errors[name] = e
        except Exception as e:
            if naive:
                raise
            errors[name] = InvalidField(
                str(e), name=name, value=getattr(ob, name, None), original=e
            )
    if errors:
        raise ValidationError(errors)
    return ob.__class__(**d)


@dataclasses.dataclass
class Ob:
    name: str
    age: int = Int(default=0)
    skills: t.List[str] = MaxItems(default_factory=list, metadata={"limit": 3})


@dataclasses.dataclass(frozen=True)
class Ob2:
    name: str
    age: int = Int(default=0)


@dataclasses.dataclass
class Ob3:
    name: str
    age: int = Int(default=0)
    father: t.Optional[Ob] = Nested(default=None)


print(Ob(name="foo", age="20"))
print(validate(Ob(name="foo", age=20)))
print(validate(Ob(name="foo", age="20")))
try:
    print(Ob(name="foo", age="foo"))
except ValidationError as e:
    print("!", e)
try:
    print(validate(Ob(name="foo", age="20", skills=["foo", "bar", "boo", "baz"])))
except ValidationError as e:
    print("!", e)


print(validate(Ob2(name="foo", age="20")))

print(validate(Ob3(name="foo", age=20)))
print(validate(Ob3(name="foo", age=20, father=Ob(name="foo", age="foo"))))
