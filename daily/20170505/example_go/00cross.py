def crossN(m, n):
    def rec(m, i, value):
        if i >= n:
            m.stmt("r = append(r, []string{{{value}}})".format(value=", ".join(value)))
        else:
            v = "v{}".format(i)
            vs = "vs{}".format(i)
            with m.for_("_, {} := range {}".format(v, vs)):
                value.append(v)
                rec(m, i + 1, value)

    args = ["vs{} []string".format(i) for i in range(n)]
    with m.func("cross{}".format(n), *args, return_="[][]string"):
        m.stmt("var r [][]string")
        rec(m, 0, [])
        m.return_("r")
    return m


if __name__ == "__main__":
    from prestring.go import Module
    m = Module()
    print(crossN(m, 5))
