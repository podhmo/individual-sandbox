import itertools
import re

scanner = re.Scanner([
    (r"\[", "["),
    (r"\]", "]"),
    (r"[^\]\[]+", lambda s, m: m),
])


def parse(title, *, scanner=scanner):
    r, rest = scanner.scan(title.strip())
    assert not rest

    itr = iter(r)
    tags = []
    buf = []
    for tk in itr:
        if tk != "[":
            buf.append(tk)

        tag = next(itr)
        tk = next(itr)
        if tk != "]":
            buf.append(tag)
            buf.append(tk)
            break
        tags.append(tag.strip())
    return tags, "".join(itertools.chain(buf, itr))


title = "[aaa][bb][memo]hello [xxx]"
print(parse(title))
