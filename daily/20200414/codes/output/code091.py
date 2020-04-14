import pygal
chart = pygal.Line()
chart.add('line', [.0002, .0005, .00035], stroke_style={'width': 5, 'dasharray': '3, 6', 'linecap': 'round', 'linejoin': 'round'})
chart.add('line', [.0004, .0009, .001], stroke_style={'width': 2, 'dasharray': '3, 6, 12, 24'})
print(chart.render(is_unicode=True))
