import pygal
chart = pygal.XY(xrange=(10, 30))
chart.add('line', [(10, .0002), (15, .0005), (12, .00035)])
print(chart.render(is_unicode=True))
