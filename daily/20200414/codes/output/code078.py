import pygal
chart = pygal.Line(show_dots=False)
chart.add('line', [.0002, .0005, .00035])
print(chart.render(is_unicode=True))
