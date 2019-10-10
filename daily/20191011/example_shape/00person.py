import shape


@shape.mark
class Person:
    name: str
    age: int


# main
print(shape.render(shape.FakeRepository([Person])))
