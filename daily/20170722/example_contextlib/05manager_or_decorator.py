import contextlib


@contextlib.contextmanager
def f():
    print("<<< before")
    yield "f"
    print(">>> after")


print("## as context manager")
with f() as v:
    print(v)


@f()
def run():  # yielded value is ignored.
    print()


print("## as decorator")
run()
