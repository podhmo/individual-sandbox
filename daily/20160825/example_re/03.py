import re
from collections import namedtuple

# TODO: rename
TokenQuery = namedtuple("TokenQuery", "rx raw")


def sliced_by_token_query(token_query, s):
    rx = token_query.rx
    r = []
    m = None
    while True:
        m = rx.search(s)
        if m is None:
            r.append(s)
            break
        else:
            r.append(s[:m.start()] + m.group(1))  # text
            r.append(m.group(2))  # separator
            s = s[m.end():]
    return r


def rx_to_token_query(rx, match_rx=re.compile("\\([^\\(\\)]+\\)")):
    m = match_rx.search(rx.pattern)
    fmt = "({prefix})({separator})(?={suffix})"
    pattern = fmt.format(
        prefix=rx.pattern[:m.start()], separator=m.group(0)[1:-1], suffix=rx.pattern[m.end():]
    )
    return TokenQuery(rx=re.compile(pattern), raw=rx)


def split_by_first_regexp_group(rx, s):
    token_query = rx_to_token_query(rx)
    return sliced_by_token_query(token_query, s)


rx = re.compile("aaa(bbb)ccc")
rx2 = re.compile("(aaa)(bbb)(?=ccc)")

s = "abc"
s = "hogehogeaaabbbcccfugafuga"
print(split_by_first_regexp_group(rx, s))
["hogehogeaaa", "bbb", "cccfugafuga"]
