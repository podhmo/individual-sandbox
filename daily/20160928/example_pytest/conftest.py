import pytest


@pytest.fixture()
def f():
    print("f")
    return "f"


@pytest.fixture(scope="module")
def ff():
    print("start m")
    yield object()
    print("end m")
