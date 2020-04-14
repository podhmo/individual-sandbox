import pygal
chart = pygal.Line(range=(.0001, .001))
chart.add('line', [.0002, .0005, .00035])
print(chart.render(is_unicode=True))
