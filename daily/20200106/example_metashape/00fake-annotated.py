import typing as t
import typing_extensions as tx


class HasMetadata(tx.Protocol):
    __metadata__: t.Dict[str, t.Any]


class Annotated:
    __slots__ = ()

    def __class_getitem__(
        cls, params: t.Tuple[t.Union[t.Type[t.Any], t.Any], ...]
    ) -> HasMetadata:
        if not isinstance(params, tuple) or len(params) < 2:
            raise TypeError(
                "Annotated[...] should be used "
                "with at least two arguments (a type and an "
                "annotation)."
            )
        msg = "Annotated[t, ...]: t must be a type."
        origin = t._type_check(params[0], msg)  # type: ignore
        if not hasattr(origin, "__metadata__"):
            origin.__metadata__ = {}
        for x in params[1:]:
            origin.__metadata__[x.__name__] = x
        return t.cast(HasMetadata, origin)


class A:
    pass


class B:
    pass


AA = Annotated[A, B]
print(AA.__metadata__)
