import pygal
chart = pygal.Pie()
for i in range(10):
  chart.add(str(i), i, inner_radius=(10 - i) / 10)
print(chart.render(is_unicode=True))
