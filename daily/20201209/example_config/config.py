from __future__ import annotations
import typing as t
from dataclasses import dataclass, replace, is_dataclass, asdict


@dataclass
class Host:
    host: str
    port: int = 3306


main = Host(host="1.2.3.4", port=9033)
replica = [
    replace(main, host="1.2.3.5"),
    replace(main, host="1.2.3.6"),
]


def MAIN() -> t.Dict[str, t.Any]:
    return {"main": main, "replica": replica}


if __name__ == "__main__":
    import json

    def dump_default(o: t.Any) -> t.Any:
        if is_dataclass(o):
            return asdict(o)
        raise TypeError(f"Object of type {type(o)} is not JSON serializable")

    print(json.dumps(MAIN(), indent=2, ensure_ascii=False, default=dump_default))
