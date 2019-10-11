import shape


@shape.mark
class Person:
    name: str
    age: int


# main
print(
    shape.translate(
        shape.Accessor(
            resolver=shape.FakeResolver(), repository=shape.FakeRepository([Person])
        )
    )
)
