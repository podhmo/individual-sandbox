import pygal
chart = pygal.Bar(style=pygal.style.styles['default'](ci_colors=(
  'black', 'blue')))
chart.add('First', [{'value': 2, 'ci': {
  'type': 'continuous', 'sample_size': 50, 'stddev': .5, 'confidence': .95}}])
chart.add('Second', [{'value': 4, 'ci': {'low': 2, 'high': 5}}])
chart.add('Third', 7)
chart.add('Fourth', [{'value': 5}])
chart.add('Fifth', [{'value': 3, 'ci': {
  'type': 'dichotomous', 'sample_size': 1000}}])
print(chart.render(is_unicode=True))
