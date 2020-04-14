import pygal
chart = pygal.Line()
chart.y_labels = [
  {'label': 'One', 'value': .0001},
  {'label': 'Three', 'value': .0003},
  {'label': 'Four', 'value': .0004},
  {'label': 'Four and a half', 'value': .00045},
  {'label': 'Five', 'value': .0005}]
chart.add('line', [.0002, .0005, .00035])
print(chart.render(is_unicode=True))
