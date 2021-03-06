from prestring.python import Module
from prestring.python._codeobject import CodeobjectModule

m = Module()
co = CodeobjectModule(m)
re = co.import_("re")
sys = co.import_("sys")

m.sep()
pattern = co.let(
    "pattern",
    re.compile(
        r"^(?P<label>DEBUG|INFO|WARNING|ERROR|CRITICAL):\s*(?P<message>\S+)",
        re.IGNORECASE,
    ),
)


with m.for_("line", sys.stdin):
    matched = co.let("matched", pattern.search(co.symbol("line")))
    with m.if_(f"{matched} is not None"):
        m.stmt("print(matched.groupdict())")
print(m)
