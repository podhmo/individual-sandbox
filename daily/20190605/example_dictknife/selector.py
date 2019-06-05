from dictknife.langhelpers import make_dict
from dictknife import Accessor
from dictknife.jsonknife import (
    get_resolver,
    access_by_json_pointer,
    json_pointer_to_path,
)


class Selector:
    def __init__(self, resolver, r=None):
        self.resolver = resolver
        self.make_dict = make_dict
        self.r = r

    def select(self, ref, *, doc=None, a=Accessor(), r=None):
        r = r or self.r
        if r is None:
            r = make_dict()  # too many?

        doc = doc or self.resolver.doc
        if not ref:
            return doc

        # xxx:
        if ref.startswith("#/"):
            ref = ref.lstrip("#")
        path = json_pointer_to_path(ref)

        accessed = access_by_json_pointer(doc, ref, guess=True)
        a.assign(r, path, accessed)
        return r


def get_selector(filename: str, *, r=None) -> Selector:
    return Selector(get_resolver(filename), r=r)
