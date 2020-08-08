from __future__ import annotations
import typing as t


class Customer:
    name: str
    address: str
    orders: t.List[Order]

    def getCreditRating(self) -> str:
        pass


class Order:
    def dispatch(self) -> None:
        pass

    def close(self) -> None:
        pass


if __name__ == "__main__":
    from emit import main
    import sys

    main(sys.modules[__name__])
