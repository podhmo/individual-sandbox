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


class CorporateCustomer(Customer):
    contactName: str
    creditRating: float
    creditLimit: int

    def billForMonth(self, n: int) -> None:
        pass

    def remind(self) -> None:
        pass


class PersonalCustomer(Customer):
    creditCardNumber: str


if __name__ == "__main__":
    from emit import main
    import sys

    main(sys.modules[__name__])
