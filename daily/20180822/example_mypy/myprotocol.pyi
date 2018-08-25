import typing as t
import typing_extensions as tx
import argparse

T = t.TypeVar("T", contravariant=True)


class ArgumentParserSubset(tx.Protocol[T]):
    def parse_args(
        self,
        argv: t.Optional[t.Sequence[str]] = ...,
        namespace: t.Optional[argparse.Namespace] = ...
    ) -> t.Any:
        ...

    # これはダメ。。
    # def add_argument(self, *name_or_flags: str, **kwargs: t.Any) -> argparse.Action:
    #     ...

    def add_argument(
        self,
        *name_or_flags: str,
        action: t.Union[str, t.Type[argparse.Action]] = ...,
        nargs: t.Union[int, str] = ...,
        const: t.Any = ...,
        default: t.Any = ...,
        type: t.Union[t.Callable[[str], T], argparse.FileType] = ...,
        choices: t.Iterable[T] = ...,
        required: bool = ...,
        help: t.Optional[str] = ...,
        metavar: t.Union[str, t.Tuple[str, ...]] = ...,
        dest: t.Optional[str] = ...,
        version: str = ...
    ) -> t.Any:
        ...
