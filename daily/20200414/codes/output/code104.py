import pygal
chart = pygal.Bar(rounded_bars=20)
chart.add('values', [3, 10, 7, 2, 9, 7])
print(chart.render(is_unicode=True))
