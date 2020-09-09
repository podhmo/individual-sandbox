from prestring.go.codeobject import Module


def gen(*, m=None):
    m = m or Module()

    m.stmt("package main")
    m.sep()

    fmt = m.import_("fmt")
    log = m.import_("log")
    errors = m.import_("github.com/pkg/errors")

    m.sep()
    m.stmt("func main() {")
    with m.scope():
        m.stmt("if err := run(); err != nil {")
        with m.scope():
            m.stmt(f'{log.Fatalf}("!!%+v", err)')
        m.stmt("}")
    m.stmt("}")
    m.sep()
    m.stmt("func run() error {")
    with m.scope():
        m.stmt("return h()")
    m.stmt("}")
    m.sep()
    m.stmt("func h() error {")
    with m.scope():
        m.stmt(f'return {errors.Wrap}(g(), "h")')
    m.stmt("}")
    m.stmt("func g() error {")
    with m.scope():
        m.stmt(f'return {errors.Wrap}(f(), "g")')
    m.stmt("}")
    m.sep()
    m.stmt("func f() error {")
    with m.scope():
        m.stmt(f'return {fmt.Errorf}("xxx")')
    m.stmt("}")
    return m


if __name__ == "__main__":
    m = gen()
    print(m)
