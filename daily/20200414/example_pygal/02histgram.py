import sys
import pygal

hist = pygal.Histogram()
hist.add("Wide bars", [(5, 0, 10), (4, 5, 13), (2, 0, 15)])
hist.add("Narrow bars", [(10, 1, 2), (12, 4, 4.5), (8, 11, 13)])
hist.render(is_unicode=True)

print(hist.render(is_unicode=True))
