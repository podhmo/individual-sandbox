from __future__ import annotations
import typing as t
import typing_extensions as tx
import dataclasses

ConfigPresetType = tx.Literal["default", "fullset"]


@dataclasses.dataclass(frozen=True)
class Config:
    is_xxx: bool = False
    is_yyy: bool = False
    is_zzz: bool = False

    @classmethod
    def from_preset(cls, preset: ConfigPresetType) -> Config:
        # if t.TYPE_CHECKING:
        #     reveal_type(cls._builtins)
        return cls(**cls._builtins[preset])

    _builtins = {
        "default": dict(),
        "fullset": dict(is_xxx=True, is_yyy=True, is_zzz=True),
    }  # type: t.ClassVar[t.Dict[ConfigPresetType, t.Dict[str, t.Any]]]


def run() -> None:
    print(Config())
    print(Config.from_preset("fullset"))


if __name__ == "__main__":
    run()
