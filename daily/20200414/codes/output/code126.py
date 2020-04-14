import pygal
chart = pygal.Bar()
chart.add('First', [{
  'value': 2,
  'label': 'This is the first',
  'xlink': {'href': 'http://en.wikipedia.org/wiki/First'}}])
print(chart.render(is_unicode=True))
