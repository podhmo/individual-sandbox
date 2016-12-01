# -*- coding:utf-8 -*-
from prestring.go import GoModule


# cross product
def cross2(m):
    with m.func("cross2", "vs0 []string", "vs1 []string", return_="[][]string"):
        m.stmt("var r [][]string")
        with m.for_("_,  v0 := range vs0"):
            with m.for_("_,  v1 := range vs1"):
                m.stmt("r = append(r, []string{v0, v1})")
        m.return_("r")
    return m

m = GoModule()
m.package("main")
print(cross2(m))
