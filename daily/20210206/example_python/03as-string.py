from __future__ import annotations
from metashape.typeinfo import typeinfo
from dataclasses import dataclass
import typing as t

if t.TYPE_CHECKING:
    from metashape.typeinfo import TypeInfo


NoneType = type(None)

# todo: None?


@dataclass
class Config:
    to_str: t.Callable[[TypeInfo], str]


class TypeStringPrinter:
    def __init__(self, config: t.Optional[Config] = None) -> None:
        self.config = config or Config(to_str=self.to_str_default)

    def to_str_default(self, info: TypeInfo) -> str:
        return info.type_.__name__

    def type_string(self, typ: t.Type[t.Any]) -> str:
        """
        >>> TypeStringPrinter().type_string(str)
        'str'
        >>> TypeStringPrinter().type_string(int)
        'int'

        >>> import typing
        >>> TypeStringPrinter().type_string(typing.Optional[int])
        'int?'

        >>> TypeStringPrinter().type_string(typing.List)
        'list[Any]'
        >>> TypeStringPrinter().type_string(typing.List[str])
        'list[str]'
        >>> TypeStringPrinter().type_string(typing.List[typing.List[str]])
        'list[list[str]]'

        >>> TypeStringPrinter().type_string(typing.List[typing.Optional[str]])
        'list[str?]'
        >>> TypeStringPrinter().type_string(typing.Optional[typing.List[str]])
        'list[str]?'

        >>> TypeStringPrinter().type_string(typing.Dict[str, int])
        'dict[str, int]'
        >>> TypeStringPrinter().type_string(typing.Dict[str, typing.Set[int]])
        'dict[str, set[int]]'

        >>> class A: pass;
        >>> TypeStringPrinter().type_string(A)
        'A'
        >>> TypeStringPrinter().type_string(t.Optional[A])
        'A?'
        """
        return self._type_string(typeinfo(typ))

    __call__ = type_string

    def _type_string(self, info: TypeInfo) -> str:
        if info.is_optional:
            return f"{self.type_string(info.type_)}?"
        if info.is_container:
            if len(info.args) == 0:
                args_str = "Any"
            else:
                args_str = ", ".join(self._type_string(x) for x in info.args)
            return f"{info.container_type}[{args_str}]"
        return self.config.to_str(info)


if __name__ == "__main__":
    import doctest

    doctest.testmod()
