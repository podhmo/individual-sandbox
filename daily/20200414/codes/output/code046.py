import pygal
chart = pygal.Line(interpolate='quadratic', interpolation_precision=3)
chart.add('line', [1, 5, 17, 12, 5, 10])
print(chart.render(is_unicode=True))
