from unittest import mock


def abc():
    return "abcc"


@mock.patch("tests.test_a.abc", return_value="aaa")
def test_it(m):
    print(abc)
    # <function abc at 0x7fe84d37cb00>
    assert "aaa" == abc()
