import pydantic


class A(pydantic.BaseModel):
    name: str = "a"


class B(pydantic.BaseModel):
    name: str = "b"
    a: A = A()

print(B())
print(dir(B()))
print(B().json())
print(type(B().json()))
