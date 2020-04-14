import pygal
config = pygal.Config()
config.style = pygal.style.DarkStyle
config.defs.append('''
  <linearGradient id="gradient-0" x1="0" x2="0" y1="0" y2="1">
    <stop offset="0%" stop-color="#ff5995" />
    <stop offset="100%" stop-color="#feed6c" />
  </linearGradient>
''')
config.defs.append('''
  <linearGradient id="gradient-1" x1="0" x2="0" y1="0" y2="1">
    <stop offset="0%" stop-color="#b6e354" />
    <stop offset="100%" stop-color="#8cedff" />
  </linearGradient>
''')
config.css.append('''inline:
  .color-0 {
    fill: url(#gradient-0) !important;
    stroke: url(#gradient-0) !important;
  }''')
config.css.append('''inline:
  .color-1 {
    fill: url(#gradient-1) !important;
    stroke: url(#gradient-1) !important;
  }''')
chart = pygal.Line(config)
chart.add('1', [1, 3, 12, 3, 4, None, 9])
chart.add('2', [7, -4, 10, None, 8, 3, 1])
chart.x_labels = ('a', 'b', 'c', 'd', 'e', 'f', 'g')
chart.legend_at_bottom = True
chart.interpolate = 'cubic'
print(chart.render(is_unicode=True))
