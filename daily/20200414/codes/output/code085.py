import pygal
chart = pygal.Line(title=u'Some different points')
chart.x_labels = ('one', 'two', 'three')
chart.add('line', [.0002, .0005, .00035])
chart.add('other line', [1000, 2000, 7000], secondary=True)
print(chart.render(is_unicode=True))
