import pygal
from pygal.style import DefaultStyle
chart = pygal.Line(no_data_text='No result found',
                   style=DefaultStyle(no_data_font_size=40))
chart.add('line', [])
print(chart.render(is_unicode=True))
