from new_directive import directive


@directive()
def hello(name: str):
    print("hello", name)


print(hello("world"))
