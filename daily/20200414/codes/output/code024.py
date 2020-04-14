import pygal
chart = pygal.Line()
chart.add('line', [.070106781, 1.414213562, 3.141592654])
chart.value_formatter = lambda x: "%.2f" % x
print(chart.render(is_unicode=True))
