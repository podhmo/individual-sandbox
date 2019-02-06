def hello(name: str, n: int) -> None:
    for i in range(n):
        print(i, "hello", name)


hello("foo", 3)
