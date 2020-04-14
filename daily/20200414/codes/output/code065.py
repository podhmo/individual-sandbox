import pygal
chart = pygal.Line(y_labels_major_every=2, show_minor_y_labels=False)
chart.add('line', [0, .0002, .0005, .00035])
print(chart.render(is_unicode=True))
