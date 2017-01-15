import hug


@hug.get("/hello")
def hello():
    return "hello"


print(vars(hug.object))
print(hello)
# print(hug.test.get("/hello", "").data)
