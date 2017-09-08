class NotEmpty:
    def is_empty(self):
        return False


class NotEmpty2:
    @property
    def is_empty(self):
        return False


if not NotEmpty().is_empty:
    print("ok")
else:
    print("oops")

if not NotEmpty2().is_empty:
    print("ok")
else:
    print("oops")
