def f() -> str:
    return ""


def use() -> None:
    z = f()
    y, z = f()
    x, y, z = f()


if __name__ == "__main__":
    import subprocess
    subprocess.check_call(["mypy", "--strict", __file__])
    print("ok")
