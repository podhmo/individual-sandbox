import pygal
chart = pygal.XY()
chart.x_labels = (.00012, .00024, .00048, .00096)
chart.add('line', [(.0002, 10), (.0005, 20), (.00035, 15)])
print(chart.render(is_unicode=True))
