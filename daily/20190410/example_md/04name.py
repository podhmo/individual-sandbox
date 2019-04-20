import re


def definition_name_from_method_name(method_name: str, _rx=re.compile(r"/.")) -> str:
    """foo/bar/boo -> fooBarBoo"""
    return _rx.sub(lambda m: m.group(0)[1].upper(), method_name)


definition_name_from_method_name("foo/bar/boo")  # => 'fooBarBoo'
