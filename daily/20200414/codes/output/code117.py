import pygal
chart = pygal.Line(title=u'Some points', x_title='X Axis')
chart.add('line', [.0002, .0005, .00035])
print(chart.render(is_unicode=True))
