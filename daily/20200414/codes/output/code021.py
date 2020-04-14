import pygal
chart = pygal.Line(min_scale=12)
chart.add('line', [1, 10, 100, 50, 25])
print(chart.render(is_unicode=True))
