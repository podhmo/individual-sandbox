def crossN(n, f, T):
    args = [T("vs{}".format(i)).slice for i in range(n)]
    r = T("r").slice.slice

    @f.func("cross{}".format(n), args=args, returns=r)
    def body(m):
        def rec(i):
            if i >= n:
                vs = ", ".join("v{}".format(i) for i in range(len(args)))
                m.stmt(
                    "{} = append({}, {}{{{}}})".
                    format(r, r, r.deslice.typename(f), vs)
                )
            else:
                with m.for_("_, v{} := range {}".format(i, args[i])):
                    rec(i + 1)

        m.stmt("var {} {}".format(r, r.typename(f)))
        rec(0)
        m.return_(r)


if __name__ == "__main__":
    from goaway import get_repository
    r = get_repository()
    f = r.package("main").file("cross.go")
    crossN(5, f, T=r.int)
    print(r.writer.write(f))
