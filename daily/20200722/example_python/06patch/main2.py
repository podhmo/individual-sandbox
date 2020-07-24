from unittest import mock

with mock.patch("f.f", return_value="xxx"):
    from b import run

    print(run())  # -> "xxx"

with mock.patch("b.f", return_value="xxx"):
    from b import run

    print(run())  # -> "xxx"
