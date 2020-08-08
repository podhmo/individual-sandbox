from __future__ import annotations
import typing as t
import typing_extensions as tx

# https://cacoo.com/ja/blog/how-to-write-class-diagram/


# for metadata
class _Label:
    def __init__(self, label: str):
        self.label = label


class 社員:
    pass


class 会社:
    所属社員: tx.Annotated[t.List[社員], _Label("所属")]
