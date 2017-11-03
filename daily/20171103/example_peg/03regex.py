import re
from nbreversible import code


rx = re.compile(
    r"""
    (?P<scheme>[a-z\d]+)
    ://
    (?:(?P<username>[a-z\d]+):(?P<password>[a-z\d\-\.]+)@)?
    (?:(?:(?P<host>[a-z\d\.]+):)?(?P<port>[\d\.]+))?
    /
    (?P<path>[a-z\d/:]+)
    """, re.IGNORECASE | re.VERBOSE
)


def parse(s, prefix="DB_", rx=rx):
    m = rx.search(s)
    if m is None:
        return None
    return {prefix + k.upper(): v for k, v in m.groupdict().items()}


"""
## examples

"""
with code():
    print(parse("postgres://USER:PASSWORD@HOST:33333/NAME"))
with code():
    print(parse("postgres://HOST:33333/NAME"))
with code():
    print(parse("postgres://USER:PASSWORD@/NAME"))
with code():
    print(parse("sqlite:///:memory:"))
with code():
    print(parse("psql://urser:un-githubbedpassword@127.0.0.1:8458/database"))
