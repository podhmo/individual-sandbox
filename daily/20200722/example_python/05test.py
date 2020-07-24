# -*- coding: utf-8 -*-
from unittest import mock


def abc():
    return "abcc"


with mock.patch("__main__.abc") as m:
    m.return_value = "aaa"
    print(abc())  # => "aaa"

print(abc())  # => abcc
