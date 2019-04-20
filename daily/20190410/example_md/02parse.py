import json
import itertools


def parse(itr):
    def _parse(*, current_lv):
        nonlocal itr
        r = []
        while True:
            for line in itr:
                lv = len(line) - len(line.lstrip(" "))
                if lv == current_lv:
                    r.append(line.lstrip())
                elif lv > current_lv:
                    itr = itertools.chain([line], itr)
                    r.append(_parse(current_lv=lv))
                    break
                else:
                    itr = itertools.chain([line], itr)
                    return r
            else:
                break
            continue
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
print(json.dumps(parse(iter(s.strip().splitlines())), indent=2))
# -- stdout --------------------
# >> [
# >>   "foo",
# >>   [
# >>     "bar",
# >>     "boo",
# >>     [
# >>       "xxx",
# >>       "yyy"
# >>     ]
# >>   ],
# >>   "moo",
# >>   [
# >>     "xoo"
# >>   ]
# >> ]
