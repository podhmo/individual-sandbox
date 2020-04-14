import pygal
chart = pygal.Line()
chart.add('line', [])
print(chart.render(is_unicode=True))
