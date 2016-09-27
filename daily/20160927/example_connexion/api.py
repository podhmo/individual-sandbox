def say_hello(name=None):
    return {"message": "Hello {}, from API!".format(name or "")}
