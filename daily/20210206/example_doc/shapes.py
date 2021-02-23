from __future__ import annotations
from metashape.declarative import field
import typing

Color = typing.Literal["Red", "Green", "Blue"]


class X:
    """これはXです"""

    name: str = field(metadata={"description": "名前です"})
    item: Item


class Y:
    name: str
    color: Color
    color2: typing.Literal["Cyan", "Magenta", "Yellow", "Black"]


class Z:
    name: str
    itemList: typing.List[Item]


class Item:
    name: str
