from unittest import mock
from b import run

with mock.patch("f.f", return_value="xxx"):
    print(run())  # -> "foo"

with mock.patch("b.f", return_value="xxx"):
    print(run())  # -> "xxx"
