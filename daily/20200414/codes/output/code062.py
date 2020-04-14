import pygal
chart = pygal.Line()
chart.y_labels_major = [.0001, .0004]
chart.add('line', [0, .0002, .0005, .00035])
print(chart.render(is_unicode=True))
