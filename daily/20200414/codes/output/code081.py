import pygal
chart = pygal.Line(stroke_style={'width': 5, 'dasharray': '3, 6', 'linecap': 'round', 'linejoin': 'round'})
chart.add('line', [.0002, .0005, .00035])
print(chart.render(is_unicode=True))
