import pygal
chart = pygal.Bar(print_values=True, value_formatter=lambda x: '{}$'.format(x))
chart.add('bar', [.0002, .0005, .00035], formatter=lambda x: '<%s>' % x)
chart.add('bar', [.0004, {'value': .0009, 'formatter': lambda x: '«%s»' % x}, .001])
print(chart.render(is_unicode=True))
