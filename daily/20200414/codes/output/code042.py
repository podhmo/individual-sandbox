import pygal
chart = pygal.Line(interpolate='hermite', interpolation_parameters={'type': 'finite_difference'})
chart.add('line', [1, 5, 17, 12, 5, 10])
print(chart.render(is_unicode=True))
