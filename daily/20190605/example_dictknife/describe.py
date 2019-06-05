import json
import os
from dictknife import loading

# todo: json pointer


def describe_dict(d, *, life=None, showzero_callable=None):
    def _describe_type(v):
        if hasattr(v, "keys"):
            return dict.__name__
        return type(v).__name__
        # if hasattr(v, "keys"):
        #     return "D"
        # elif isinstance(v, (list, tuple)):
        #     return "L"
        # elif isinstance(v, bool):
        #     return "B"
        # elif isinstance(v, str):
        #     return "S"
        # elif isinstance(v, int):
        #     return "I"
        # elif isinstance(v, float):
        #     return "F"
        # elif v is None:
        #     return "?"
        # else:  # any
        #     return "*"

    def _show_on_lifezero(d):
        if hasattr(d, "keys"):
            keys = []
            for k, v in sorted(d.items()):
                if hasattr(v, "keys"):
                    keys.append(f"{k}:{_describe_type(v)}@{len(v)}")
                elif isinstance(v, (list, tuple)):
                    keys.append(f"{k}:{_describe_type(v)}@{len(v)}")
                else:
                    keys.append(f"{k}:{_describe_type(v)}")
            return {"$keys": keys}
        elif isinstance(d, (list, tuple)):
            seen = {}
            for x in d:
                rep = _show(x, life=0)
                sig = json.dumps(rep, sort_keys=True, default=str)
                if sig in seen:
                    continue
                seen[sig] = rep
            return {"$len": len(d), "$cases": list(seen.values())}
        else:
            return _describe_type(v)

    def _show(d, *, life, showzero_callable=showzero_callable or _show_on_lifezero):
        if life <= 0:
            return showzero_callable(d)

        if hasattr(d, "keys"):
            keys = []
            children = {}
            for k, v in sorted(d.items()):
                if hasattr(v, "__len__") and len(v) == 0:
                    keys.append(f"{k}:{_describe_type(v)}@{len(v)}")
                elif hasattr(v, "keys"):
                    children[k] = _show(v, life=life - 1)
                elif isinstance(v, (list, tuple)):
                    children[k] = _show(v, life=life)  # -1?
                else:
                    keys.append(f"{k}:{_describe_type(v)}")
            r = {}
            if keys:
                r["$keys"] = keys
            if children:
                r["$children"] = children
            return r
        elif isinstance(d, (list, tuple)):
            seen = {}
            members = []
            for x in d:
                rep = _show(x, life=life - 1)
                members.append(rep)
                sig = json.dumps(rep, sort_keys=True, default=str)
                if sig in seen:
                    continue
                seen[sig] = rep
            r = {"$len": len(d)}
            if seen:
                r["$cases"] = list(seen.values())
            if members:
                r["$members"] = members
            return r
        else:
            return d
    if life is None:
        life = int(os.environ.get("LIFE") or "0")
    return _show(d, life=life)


def p(d):
    print()
    loading.dumpfile(d, format="json")
    print()
