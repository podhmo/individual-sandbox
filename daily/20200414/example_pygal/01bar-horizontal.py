import sys
import pygal

bar_chart = pygal.HorizontalBar()
bar_chart.title = "Browser usage in February 2012 (in %)"
bar_chart.add("IE", 19.5)
bar_chart.add("Firefox", 36.6)
bar_chart.add("Chrome", 36.3)
bar_chart.add("Safari", 4.5)
bar_chart.add("Opera", 2.3)

print(bar_chart.render(is_unicode=True))
