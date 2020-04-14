import pygal
chart = pygal.Line(fill=True, zero=.0004)
chart.add('line', [.0002, .0005, .00035])
print(chart.render(is_unicode=True))
