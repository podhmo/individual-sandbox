import pygal
chart = pygal.Line()
chart.y_labels = .0001, .0003, .0004, .00045, .0005
chart.add('line', [.0002, .0005, .00035])
print(chart.render(is_unicode=True))
