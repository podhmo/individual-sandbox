import pygal
interrupted_chart = pygal.Line()
interrupted_chart.add(
  'Temperature', [22, 34, 43, 12, None, 12, 55, None, 56],
  allow_interruptions=True)
interrupted_chart.add(
  'Temperature', [11, 17, 21.5, 6, None, 6, 27.5, None, 28])
print(interrupted_chart.render(is_unicode=True))
