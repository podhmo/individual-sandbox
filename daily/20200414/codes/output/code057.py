import pygal
chart = pygal.Line(x_label_rotation=20)
chart.x_labels = [
    'This is the first point !',
    'This is the second point !',
    'This is the third point !',
    'This is the fourth point !']
chart.x_labels_major = ['This is the first point !', 'This is the fourth point !']
chart.add('line', [0, .0002, .0005, .00035])
print(chart.render(is_unicode=True))
