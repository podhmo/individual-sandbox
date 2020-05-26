import typing as t


class Quotient:
    quo: int
    rem: int


class RPCError:
    code: int
    message: str


class ArithService:
    def divide(self, a: int, b: int) -> t.Union[t.Optional[Quotient], RPCError]:
        pass

    def multiply(self, a: int, b: int) -> t.Union[int, RPCError]:
        """Multiply multiples two digits and returns result."""
        pass

    def pow(self, base: float, exp: t.Optional[float]) -> t.Union[float, RPCError]:
        """Pow returns x**y, the base-x exponential of y. If Exp is not set then default value is 2."""
        pass

    def sum(self, a: int, b: int = 2) -> t.Union[int, RPCError]:
        """Sum sums two digits and returns error with error code as result and IP from context."""
        pass
