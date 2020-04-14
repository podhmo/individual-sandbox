import pygal
chart = pygal.Line(secondary_range=(10, 25))
chart.add('primary', [.0002, .0005, .00035])
chart.add('secondary', [10, 15, 12], secondary=True)
print(chart.render(is_unicode=True))
