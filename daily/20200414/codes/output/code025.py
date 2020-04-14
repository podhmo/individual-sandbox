import pygal
chart = pygal.XY()
chart.add('line', [(12, 31), (8, 28), (89, 12)])
chart.x_value_formatter = lambda x:  '%s%%' % x
print(chart.render(is_unicode=True))
