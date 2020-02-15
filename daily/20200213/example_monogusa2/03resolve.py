from monogusa import component
from monogusa.dependencies import resolve_args


@component
def one() -> int:
    return 1


def use(one: int) -> None:
    print(one)


print("!", resolve_args(use))
# ! [1]
