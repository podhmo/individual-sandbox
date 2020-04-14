import pygal
chart = pygal.XY()
chart.x_labels = ({
  'label': 'Twelve',
  'value': .00012
}, {
  'label': 'Twenty four',
  'value': .00024
}, {
  'label': 'Forty eight',
  'value': .00048
}, {
  'label': 'Ninety six',
  'value': .00096})
chart.add('line', [(.0002, 10), (.0005, 20), (.00035, 15)])
print(chart.render(is_unicode=True))
