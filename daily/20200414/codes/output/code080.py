import pygal
chart = pygal.Line(dots_size=5)
chart.add('line', [.0002, .0005, .00035])
print(chart.render(is_unicode=True))
