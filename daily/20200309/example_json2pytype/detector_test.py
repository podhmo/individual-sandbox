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


