import pygal
chart = pygal.Line(logarithmic=True)
values = [1, 3, 43, 123, 1231, 23192]
chart.x_labels = map(str, values)
chart.add('log example', values)
print(chart.render(is_unicode=True))
