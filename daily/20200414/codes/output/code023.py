import pygal
chart = pygal.Line(order_min=1)
chart.add('line', [1, 10, 100, 50, 25])
print(chart.render(is_unicode=True))
