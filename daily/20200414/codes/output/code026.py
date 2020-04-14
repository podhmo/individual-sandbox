import pygal
from pygal.style import DefaultStyle
chart = pygal.Bar(print_values=True, style=DefaultStyle(
                  value_font_family='googlefont:Raleway',
                  value_font_size=30,
                  value_colors=('white',)))
chart.add('line', [0, 12, 31, 8, -28, 0])
print(chart.render(is_unicode=True))
