from prestring.go import Module
m = Module()
m.package('main')

with m.import_group() as im:
    im.import_("fmt")

with m.func('Fizzbuzz', 'v int', return_='(string, error)'):
    with m.if_("v == 1"):
        m.return_('"1", nil')
    for i in range(2, 101):
        m.unnewline()
        m.append("else ")
        with m.if_("v == {}".format(i)):
            if i % 15 == 0:
                m.return_('"fizzbuzz", nil')
            elif i % 3 == 0:
                m.return_('"fizz", nil')
            elif i % 5 == 0:
                m.return_('"buzz", nil')
            else:
                m.return_('"{}", nil'.format(i))
    with m.else_():
        m.return_('"", fmt.Errorf("unsupported value: %q", v)')

print(m)
