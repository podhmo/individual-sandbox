import pygal
chart = pygal.Line()
chart.add('line', [.0002, .0005, .00035], dots_size=4)
chart.add('line', [.0004, .0009, .001], dots_size=12)
print(chart.render(is_unicode=True))
