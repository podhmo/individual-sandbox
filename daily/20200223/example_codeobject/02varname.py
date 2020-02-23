from prestring.python import Module

m = Module()
m.import_("math")
m.sep()

with m.def_('rmse', 'xs', 'ys'):
    m.stmt('acc = 0')
    m.stmt('assert len(xs) == len(ys)')
    with m.for_('x, y', 'zip(xs, ys)'):
        m.stmt('acc += (x - y) ** 2')
    m.return_('math.sqrt(acc / len(xs))')

# m.stmt('xs = [92, 95, 110, 114, 100, 98, 93]')
# m.stmt('ys = [95, 93, 100, 114, 105, 100, 96]')
print(m)
