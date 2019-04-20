import json
import itertools


def parse(itr):
    def _parse(current_level):
        nonlocal itr
        r = []
        buf = []
        while True:
            for line in itr:
                if line.startswith("#"):
                    level = len(line) - len(line.lstrip("#"))
                    if level == current_level:
                        if buf:
                            r.append(buf)
                        buf = [line]
                    elif level < current_level:
                        if buf:
                            r.append(buf)
                        itr = itertools.chain([line], itr)
                        return r
                    else:  # level > current_level:
                        buf.append(_parse(current_level=level))
                        buf[-1].insert(0, line)
                        break
                else:
                    buf.append(line)
            else:
                break
            continue
        if buf:
            r.append(buf)
        return r

    return _parse(current_level=0)


with open("input.md") as rf:
    r = parse(rf)
    print(json.dumps(r, indent=2))
