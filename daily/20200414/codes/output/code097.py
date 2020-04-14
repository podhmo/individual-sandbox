import pygal
chart = pygal.Bar(height=100)
chart.add('1', 1)
chart.add('2', 2)
print(chart.render(is_unicode=True))
