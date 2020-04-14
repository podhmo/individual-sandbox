import pygal
chart = pygal.Line(human_readable=True)
chart.add('line', [0, .0002, .0005, .00035])
print(chart.render(is_unicode=True))
