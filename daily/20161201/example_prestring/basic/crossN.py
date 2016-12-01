# -*- coding:utf-8 -*-
from prestring.go import GoModule


# cross product
def crossN(m, n):
    args = ["vs{} []string".format(i) for i in range(n)]

    def rec(m, i, value):
        if i >= n:
            m.stmt("r = append(r, []string{{{value}}})".format(value=", ".join(value)))
        else:
            v = "v{}".format(i)
            vs = "vs{}".format(i)
            with m.for_("_, {} := range {}".format(v, vs)):
                value.append(v)
                rec(m, i + 1, value)

    with m.func("cross{}".format(n), *args, return_="[][]string"):
        m.stmt("var r [][]string")
        rec(m, 0, [])
        m.return_("r")
    return m

m = GoModule()
m.package("main")
print(crossN(m, 5))
