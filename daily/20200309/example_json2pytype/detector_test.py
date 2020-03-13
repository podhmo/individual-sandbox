import typing as t
from detector import Detector, Result


def test_detect_dict():
    from detector import Object

    d = {"name": "foo", "age": 20}
    detector = Detector()
    info = detector.detect_dict(d, path=[], result=Result())

    assert isinstance(info, Object)
    assert info.size == 2
    assert info.props["name"].type == str
    assert info.props["age"].type == int


def test_detect_dict__zero():
    from detector import Object

    d = {}
    detector = Detector()
    info = detector.detect_dict(d, path=[], result=Result())

    assert isinstance(info, Object)
    assert info.size == 0


def test_detect_dict_many():
    from detector import Object

    d0 = {"name": "foo", "age": 20}
    d1 = {"name": "boo", "age": 20, "nickname": "B"}
    d2 = {"name": "bar", "age": 20}
    candidates = [d0, d1, d2]

    detector = Detector()
    info = detector.detect_dict_many(candidates, path=[], result=Result())

    assert isinstance(info, Object)
    assert info.size == 3
    assert info.props["name"].type == str
    assert info.props["age"].type == int
    assert info.props["nickname"].base == t.Optional
    assert info.props["nickname"].item.type == str


def test_detect_dict_many2():
    from detector import Object

    d0 = {"x": "X", "y": "Y"}
    d1 = {"x": "X", "z": "Z"}
    candidates = [d0, d1]

    detector = Detector()
    info = detector.detect_dict_many(candidates, path=[], result=Result())

    assert isinstance(info, Object)
    assert info.size == 3
    assert info.props["x"].type == str
    assert info.props["y"].item.type == str
    assert info.props["y"].base == t.Optional
    assert info.props["z"].item.type == str
    assert info.props["z"].base == t.Optional


def test_detect_dict_many3():
    from detector import Object

    d0 = {"x": "X", "y": "Y"}
    d1 = {"x": "X"}
    candidates = [d0, d1]

    detector = Detector()
    info = detector.detect_dict_many(candidates, path=[], result=Result())

    assert isinstance(info, Object)
    assert info.size == 2
    assert info.props["x"].type == str
    assert info.props["y"].item.type == str
    assert info.props["y"].base == t.Optional


def test_detect_list():
    from detector import Container, Object

    d = [{"name": "foo"}, {"name": "boo", "nickname": "B"}]

    detector = Detector()
    info = detector.detect_list(d, path=[], result=Result())

    assert isinstance(info, Container)
    assert info.base == t.List
    assert info.size == 2

    assert isinstance(info.item, Object)
    assert info.item.size == 2
    assert info.item.props["name"].type == str
    assert info.item.props["nickname"].item.type == str


def test_detect_list__zero():
    from detector import Container, Object

    d = []

    detector = Detector()
    info = detector.detect_list(d, path=[], result=Result())

    assert isinstance(info, Container)
    assert info.base == t.List
    assert info.size == 0

    assert isinstance(info.item, Object)
    assert info.item.size == 0
