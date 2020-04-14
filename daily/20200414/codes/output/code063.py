import pygal
chart = pygal.Line(y_label_rotation=20, y_labels_major_every=3)
chart.add('line', [0, .0002, .0005, .00035])
print(chart.render(is_unicode=True))
