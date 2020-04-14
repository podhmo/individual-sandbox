import pygal
chart = pygal.Line(show_x_labels=False)
chart.x_labels = 'Red', 'Blue', 'Green'
chart.add('line', [.0002, .0005, .00035])
print(chart.render(is_unicode=True))
