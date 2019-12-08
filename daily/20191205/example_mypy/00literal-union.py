import typing as t
import typing_extensions as tx

f0: tx.Literal[False] = False
reveal_type(f0)

f1: t.Union[int, tx.Literal[False]] = False
reveal_type(f1)

f2: t.Union[int, tx.Literal[False]] = 10
reveal_type(f2)

f3: t.Union[int, tx.Literal[False]] = True  # hmm?
reveal_type(f3)

# fmt: off
@t.overload
def fetch(n: int) -> t.Literal["INT"]: ...
@t.overload
def fetch(n: int, *, flag: bool) -> t.Literal["BOOL"]: ...
def fetch(n: int, *, flag: t.Optional[bool]=False) -> t.Literal["INT", "BOOL"]:
# fmt: on
    if flag is True:
        return "BOOL"
    return "INT"

reveal_type(fetch(10))
reveal_type(fetch(10, flag=False))
reveal_type(fetch(10, flag=True))

# fmt: off
@t.overload
def g(x: tx.Literal[False]) -> tx.Literal["FALSE"]: ...
@t.overload
def g(x: int) -> tx.Literal["INT"]: ...
def g(x: t.Union[tx.Literal[False], int]) ->tx.Literal["FALSE", "INT"]:
# fmt: on
    if x is False:
        return "FALSE"
    return "INT"
