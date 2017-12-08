import contextlib


@contextlib.contextmanager
def f(x):
    print("before")
    yield x
    print("after")


def call_without_enter():
    f("hai")


def call_with_enter():
    with f("hai") as x:
        print(x)


def main():
    call_without_enter()
    print("----------------------------------------")
    call_with_enter()


if __name__ == "__main__":
    main()
