import pygal
chart = pygal.Line()
chart.add('line', [.0002, .0005, .00035], show_dots=False)
chart.add('line', [.0004, .0009, .001])
print(chart.render(is_unicode=True))
