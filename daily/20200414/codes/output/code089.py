import pygal
chart = pygal.Line()
chart.add('line', range(12))
chart.add('line', range(12)[::-1], show_only_major_dots=True)
chart.x_labels = map(str, range(12))
chart.x_labels_major = ['2', '4', '8', '11']
print(chart.render(is_unicode=True))
