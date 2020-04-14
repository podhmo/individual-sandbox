import pygal
chart = pygal.Line(legend_box_size=18)
chart.add('Serie 1', [1, 2, 3])
chart.add('Serie 2', [4, 2, 0])
chart.add('Serie 3', [1, -1, 1])
chart.add('Serie 4', [3, 1, 5])
print(chart.render(is_unicode=True))
