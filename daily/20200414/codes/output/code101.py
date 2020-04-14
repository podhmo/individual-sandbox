import pygal
chart = pygal.Bar(margin_right=50)
chart.x_labels = u'αβγδ'
chart.add('line 1', [5, 15, 10, 8])
chart.add('line 2', [15, 20, 8, 11])
print(chart.render(is_unicode=True))
