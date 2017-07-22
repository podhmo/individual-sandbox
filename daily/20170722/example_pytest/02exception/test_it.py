import pytest


@pytest.fixture
def ob():
    ob = object()
    print("")
    print("<<< before", id(ob))
    yield ob
    print(">>> after", id(ob))


def test_it(ob):
    print("**test", id(ob), "**")
    1 / 0
