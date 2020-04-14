import pygal
chart = pygal.Bar(print_labels=True)
chart.add('line', [
  0,
  {'value': 12, 'label': 'Twelve'},
  31,
  {'value': 8, 'label': 'eight'},
  28,
  0
])
print(chart.render(is_unicode=True))
