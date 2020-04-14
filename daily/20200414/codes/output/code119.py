import pygal
chart = pygal.Line(tooltip_border_radius=10)
chart.add('line', [.0002, .0005, .00035])
print(chart.render(is_unicode=True))
