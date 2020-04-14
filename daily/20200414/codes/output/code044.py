import pygal
chart = pygal.Line(interpolate='hermite', interpolation_parameters={'type': 'kochanek_bartels', 'b': -1, 'c': 1, 't': 1})
chart.add('line', [1, 5, 17, 12, 5, 10])
print(chart.render(is_unicode=True))
