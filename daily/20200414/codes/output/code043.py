import pygal
chart = pygal.Line(interpolate='hermite', interpolation_parameters={'type': 'cardinal', 'c': .75})
chart.add('line', [1, 5, 17, 12, 5, 10])
print(chart.render(is_unicode=True))
