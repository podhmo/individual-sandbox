class Err:
    __slots__ = ("value", )

    def __init__(self, value):
        self.value = value

    def __bool__(self):
        return False

    def __repr__(self):
        return "Err<{}>".format(self.value)

    def __eq__(self, x):
        if not hasattr(x, "value"):
            return False
        return self.value == x.value


def is_ok(d):
    x = d.get("x")
    if x is None:
        return Err("x is not found")
    if x["status"] != "valid":
        return Err("x is invalid")
    y = x.get("y")
    if y is None:
        return Err("y is not found")
    if y["status"] != "valid":
        return Err("y is invalid")
    return True


print(is_ok({"x": {"status": "invalid"}}))
# Err<x is invalid>
