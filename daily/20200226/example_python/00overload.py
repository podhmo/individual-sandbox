from typing import Optional
import dataclasses
from typing import Union, overload

# https://mypy.readthedocs.io/en/stable/more_types.html#function-overloading

# Overload *variants* for 'mouse_event'.
# These variants give extra information to the type checker.
# They are ignored at runtime.


@dataclasses.dataclass
class ClickEvent:
    x: int
    y: int


@dataclasses.dataclass
class DragEvent:
    x0: int
    y0: int
    x1: int
    y1: int


@overload
def mouse_event(x1: int, y1: int) -> ClickEvent:
    ...


@overload
def mouse_event(x1: int, y1: int, x2: int, y2: int) -> DragEvent:
    ...


# The actual *implementation* of 'mouse_event'.
# The implementation contains the actual runtime logic.
#
# It may or may not have type hints. If it does, mypy
# will check the body of the implementation against the
# type hints.
#
# Mypy will also check and make sure the signature is
# consistent with the provided variants.


def mouse_event(
    x1: int, y1: int, x2: Optional[int] = None, y2: Optional[int] = None
) -> Union[ClickEvent, DragEvent]:
    if x2 is None and y2 is None:
        return ClickEvent(x1, y1)
    elif x2 is not None and y2 is not None:
        return DragEvent(x1, y1, x2, y2)
    else:
        raise TypeError("Bad arguments")
