import pygal
chart = pygal.Bar()
chart.add('Serie', [
 {'value': 2}, 3, 4,
 {'value': 10, 'color': 'red'},
 {'value': 11, 'color': 'rgba(255, 45, 20, .6)'}, 4, 2
])
print(chart.render(is_unicode=True))
