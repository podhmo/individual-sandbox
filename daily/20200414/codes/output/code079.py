import pygal
chart = pygal.Line(show_only_major_dots=True)
chart.add('line', range(12))
chart.x_labels = map(str, range(12))
chart.x_labels_major = ['2', '4', '8', '11']
print(chart.render(is_unicode=True))
