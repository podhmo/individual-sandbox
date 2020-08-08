class Customer:
    name: str
    address: str

    def getCreditRating(self) -> str:
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
