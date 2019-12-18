from __future__ import annotations


class Code:
    def __getattr__(self, name: str) -> Attr:
        return Attr(name, self)


class Attr:
    def __init__(self, name: str, co: Code):
        self.name = name
        self._co = co
