class MyKeyError(KeyError):
    pass


try:
    raise MyKeyError("oops")
except KeyError as e:
    print(f"! {e!r}")
