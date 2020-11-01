from __future__ import annotations
import typing as t

if hasattr(t, "Annotated"):
    Annotated = t.Annotated
else:
    import typing_extensions as tx

    Annotated = tx.Annotated


class Description:
    def __init__(self, description: str) -> None:
        self.description = description

    def __repr__(self) -> repr:
        return f"Description({self.description})"


class WithAnnotated:
    name: Annotated[str, Description("name of object")]  # noqa: F722


print(t.get_type_hints(WithAnnotated))
