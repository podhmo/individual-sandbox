import typing as t
import typing_extensions as tx


class Metadata(tx.TypedDict):
    name: str


def use(d: t.Dict[str, t.Any]) -> None:
    pass


metadata: Metadata = {"name": "foo"}
use(metadata)
