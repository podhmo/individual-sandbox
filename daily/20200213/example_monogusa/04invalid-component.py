from monogusa import component


class F:
    pass


class G:
    pass

@component
def f() -> F:
    return F()


def use(f: G) -> None:
    print(f)


if __name__ == "__main__":
    from monogusa.cli import run

    run()
