def f(name: str, *args: str) -> None:
    print(name, args)

# TypeError: f() got multiple values for argument 'name'
f(1, 2, 3, name="foo")
