import json
import os
from dictknife.jsonknife import path_to_json_pointer
from dictknife import loading

# todo: json pointer


def describe_dict(d, *, life=None, showzero_callable=None):
    sigmap = {}

    def _describe_type(v):
        if hasattr(v, "keys"):
            return dict.__name__
        return type(v).__name__

    def _show_on_lifezero(d, *, path):
        if hasattr(d, "keys"):
            rep = {}
            for k, v in sorted(d.items()):
                if hasattr(v, "keys"):
                    rep[k] = f"{_describe_type(v)}@{len(v)}"
                elif isinstance(v, (list, tuple)):
                    rep[k] = f"{_describe_type(v)}@{len(v)}"
                else:
                    rep[k] = f"{_describe_type(v)}"
            sig = json.dumps(rep, sort_keys=True, default=str)
            if sig in sigmap:
                return sigmap[sig]
            sigmap[sig] = path_to_json_pointer(path)
            return rep

        elif isinstance(d, (list, tuple)):
            seen = {}
            for x in d:
                rep = _show(x, life=0, path=path)
                sig = json.dumps(rep, sort_keys=True, default=str)
                if sig in seen:
                    continue
                seen[sig] = rep
            return {"$len": len(d), "$cases": list(seen.values())}
        else:
            return _describe_type(v)

    def _show(
        d, *, path, life, showzero_callable=showzero_callable or _show_on_lifezero
    ):
        if life <= 0:
            return showzero_callable(d, path=path)

        if hasattr(d, "keys"):
            rep = {}
            for k, v in sorted(d.items()):
                path.append(k)
                if hasattr(v, "__len__") and len(v) == 0:
                    rep[k] = f"{_describe_type(v)}@{len(v)}"
                elif hasattr(v, "keys"):
                    rep[k] = _show(v, life=life - 1, path=path)
                elif isinstance(v, (list, tuple)):
                    rep[k] = _show(v, life=life - 1, path=path)
                else:
                    rep[k] = f"{_describe_type(v)}"
                path.pop()
            sig = json.dumps(rep, sort_keys=True, default=str)
            if sig in sigmap:
                return sigmap[sig]
            sigmap[sig] = path_to_json_pointer(path)
            return rep
        elif isinstance(d, (list, tuple)):
            seen = {}
            members = []
            path.append("[]")

            csigmap = {}
            for x in d:
                rep = _show(x, life=life - 1, path=path)
                sig = json.dumps(rep, sort_keys=True, default=str)

                if sig in seen:
                    members.append(csigmap[sig])
                    continue
                seen[sig] = rep
                csigmap[sig] = f"{path_to_json_pointer(path)}/$cases/{len(csigmap)}"
                members.append(csigmap[sig])
            rep = {"$len": len(d)}
            if seen:
                rep["$cases"] = list(seen.values())
            if members:
                rep["$members"] = members
            path.pop()
            sig = json.dumps(rep, sort_keys=True, default=str)
            if sig in sigmap:
                return sigmap[sig]
            sigmap[sig] = path_to_json_pointer(path)
            return rep
        else:
            return _describe_type(v)

    if life is None:
        life = int(os.environ.get("LIFE") or "0")
    rep = _show(d, life=life, path=["#"])
    return rep


def p(d):
    print()
    loading.dumpfile(d, format="json")
    print()
