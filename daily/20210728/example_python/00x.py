from typing import Any

def f() -> Any:
    return 1

reveal_type(f)
reveal_type(any)