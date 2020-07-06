import typing as t
import typing_extensions as tx
from metashape._access import iterate_props, SeeArgsForMetadata


class description:
    def __init__(self, description: str) -> None:
        self.description = description


class description2:
    def __init__(self, message: str) -> None:
        self.message = message

    def as_metadata(self) -> t.Dict[str, t.Any]:
        return {"description": self.message}


class WithAnnotated:
    name: tx.Annotated[str, description("name")]
    name2: tx.Annotated[str, description2("name")]  # as_metadata

    nickname: tx.Annotated[t.Optional[str], description("nickname")]
    nickname2: t.Optional[tx.Annotated[str, description("nickname2")]]

    parents: tx.Annotated[t.List[str], description("parents")]
    parents2: t.List[tx.Annotated[str, description("parents")]]  # not extract
    parents3: tx.Annotated[
        t.List[tx.Annotated[str, description("parents")]], SeeArgsForMetadata()
    ]

    complex_nested: t.Dict[
        str, t.Dict[str, tx.Annotated[str, description("complex_nested")]]
    ]
    complex_nested2: tx.Annotated[
        t.Dict[str, t.Dict[str, tx.Annotated[str, description("complex_nested")]]],
        SeeArgsForMetadata(),
    ]


for row in iterate_props(WithAnnotated):
    print(row)

