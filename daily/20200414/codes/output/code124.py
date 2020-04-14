import pygal
chart = pygal.Line()
chart.add('Serie', [
  {'value': 1, 'node': {'r': 2}},
  {'value': 2, 'node': {'r': 4}},
  {'value': 3, 'node': {'r': 6}},
  {'value': 4, 'node': {'r': 8}}
])
print(chart.render(is_unicode=True))
