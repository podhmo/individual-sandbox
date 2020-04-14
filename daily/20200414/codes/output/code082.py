import pygal
chart = pygal.Line(show_x_guides=True)
chart.x_labels = ['alpha', 'beta', 'gamma']
chart.add('line', [.0002, .0005, .00035])
print(chart.render(is_unicode=True))
