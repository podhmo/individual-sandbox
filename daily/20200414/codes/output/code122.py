import pygal
chart = pygal.Bar()
chart.add('Serie', [
 {'value': 2}, 3, 4,
 {'value': 10, 'style': 'fill: red; stroke: black; stroke-width: 4'},
 {'value': 11, 'style': 'fill: rgba(255, 45, 20, .6); stroke: black; stroke-dasharray: 15, 10, 5, 10, 15'},
 4, 2
])
print(chart.render(is_unicode=True))
