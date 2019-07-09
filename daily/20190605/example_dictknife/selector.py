from dictknife.langhelpers import make_dict
from dictknife import Accessor
from dictknife.accessing import ImmutableModifier
from dictknife.jsonknife import access_by_json_pointer, json_pointer_to_path


class Selector:
    def __init__(self, resolver, *, wrap: bool, r=None):
        self.resolver = resolver
        self.make_dict = make_dict
        self.wrap = wrap
        self.r = r

    def select(self, ref, *, doc=None, a=Accessor(), r=None):
        ref_wrap = ref if self.wrap else None
        doc = doc or self.resolver.doc

        r = r or self.r
        if r is None:
            r = make_dict()  # too many?

        if "@" in ref:
            ref, ref_wrap = ref.split("@", 1)

        if ref:
            accessed = access_by_json_pointer(doc, ref, guess=True)
        else:
            accessed = doc

        if ref_wrap is None:
            return accessed

        wrap_path = json_pointer_to_path(ref_wrap)
        a.assign(r, wrap_path, accessed)
        return r


def transform_dict_with_respect_index(data, *, modifier=ImmutableModifier()):
    def _map(d):
        if isinstance(d, (list, tuple)):
            return modifier.modify_list(_map, d)
        elif hasattr(d, "keys"):
            if any(not k.isdigit() for k in d.keys()):
                return modifier.modify_dict(_map, d)
            else:
                max_k = max(d.keys(), key=int)
                return [
                    None if str(i) not in d else _map(d[str(i)])
                    for i in range(int(max_k) + 1)
                ]
        else:
            return d

    return _map(data)
