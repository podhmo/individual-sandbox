import typing as t


def g() -> t.Tuple[t.Mapping]:
    return ({}, )


def use() -> None:
    z = g()
    y, z = g()  # ng
    x, y, z = g()  # ng


if __name__ == "__main__":
    import subprocess
    subprocess.check_call(["mypy", "--strict", __file__])
    print("ok")
