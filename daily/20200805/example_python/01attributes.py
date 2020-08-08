class Customer:
    name: str
    address: str

    def getCreditRating(self) -> str:
        pass


if __name__ == "__main__":
    from emit import main
    import sys

    main(sys.modules[__name__])
