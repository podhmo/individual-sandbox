import pygal
chart = pygal.Bar()
for i in range(10):
  chart.add(str(i), i, rounded_bars=2 * i)
print(chart.render(is_unicode=True))
