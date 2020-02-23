from prestring.python import Module

m = Module()

re = m.import_("re")
sys = m.import_("sys")

m.sep()
m.stmt(
    "pattern = {}",
    re.compile(
        r"^(?P<label>DEBUG|INFO|WARNING|ERROR|CRITICAL):\s*(?P<message>\S+)",
        re.IGNORECASE,
    ),
)

with m.for_("line", sys.stdin):
    m.stmt("m = pattern.search(line)")
    with m.if_("m is not None"):
        m.stmt("print(m.groupdict())")
print(m)
