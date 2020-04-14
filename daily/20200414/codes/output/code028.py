import pygal
chart = pygal.Bar(print_values=True, print_values_position='top')
chart.add('line', [0, 12, 31, 8, -28, 0])
print(chart.render(is_unicode=True))
