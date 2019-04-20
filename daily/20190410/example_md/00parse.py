import json
import itertools


def parse(itr, r, *, current_level):
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
                    return r, itertools.chain([line], itr)
                else:  # level > current_level:
                    subr, itr = parse(itr, [line], current_level=level)
                    buf.append(subr)
                    break
            else:
                buf.append(line)
        else:
            break
        continue
    if buf:
        r.append(buf)
    return r, itr


with open("input.md") as rf:
    r, _ = parse(rf, [], current_level=1)
    print(json.dumps(r, indent=2))
