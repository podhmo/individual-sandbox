import json
import datetime
import difflib


def _default_tostring(d, default=str):
    return json.dumps(d, indent=2, ensure_ascii=False, sort_keys=True, default=default)


def diff(d0, d1, tostring=_default_tostring, fromfile="left", tofile="right", n=3, terminator="\n"):
    """fancy diff"""
    s0 = tostring(d0).split(terminator)
    s1 = tostring(d1).split(terminator)
    return difflib.unified_diff(s0, s1, fromfile=fromfile, tofile=tofile, lineterm="", n=n)


def uppercase(d):
    if isinstance(d, dict):
        return {k.upper(): uppercase(v) for k, v in d.items()}
    elif isinstance(d, (list, tuple)):
        return [uppercase(x) for x in d]
    else:
        return d


person = {"name": "foo", "age": 10, "createdAt": datetime.date(2000, 1, 1)}
person2 = uppercase(person)
print("\n".join(diff(person, person2)))
