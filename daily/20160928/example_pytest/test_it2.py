# -*- coding:utf-8 -*-
import pytest
# kind of scope?


@pytest.fixture(scope="module")
def m():
    return object()


@pytest.fixture(scope="module")
def k():
    print("start")
    yield object()
    print("end")


@pytest.fixture(scope="function")
def kk():
    print("start f")
    yield object()
    print("end f")


@pytest.fixture(scope="session")
def kkk():
    print("start s")
    yield object()
    print("end s")


def test_ok(m, k, kk, kkk, ff):
    print(m)
    assert 1 == 1


def test_ng(m, k, kk, kkk, ff):
    print(m)
    assert 1 == 1


@pytest.mark.usefixtures("kkk", "f", autouse=True)
class Test:
    def test_it(self, kkk, f):
        print(kkk)
        print(f)
        assert 1 == 1
