import json
import itertools


class PushBackIterator:
    def __init__(self, itr):
        self.itr = iter(itr)

    def push_back(self, line):
        self.itr = itertools.chain([line], self.itr)

    def __next__(self):
        return next(self.itr)

    def __iter__(self):
        return self


def parse(itr):
    def _parse(*, current_lv):
        r = []
        for line in itr:
            lv = len(line) - len(line.lstrip(" "))
            if lv == current_lv:
                r.append(line.lstrip())
            elif lv > current_lv:
                itr.push_back(line)
                r.append(_parse(current_lv=lv))
            else:
                itr.push_back(line)
                return r
        return r

    return _parse(current_lv=0)


s = """
foo
  bar
  boo
    xxx
    yyy
moo
  xoo
"""
print(json.dumps(parse(PushBackIterator(s.strip().splitlines())), indent=2))
