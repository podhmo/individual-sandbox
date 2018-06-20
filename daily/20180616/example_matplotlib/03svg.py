from matplotlib.figure import Figure
from matplotlib.backends.backend_svg import new_figure_manager


def graph(fig):
    t1 = [1, 2, 3, 4, 5]
    ax = fig.add_subplot(1, 1, 1)
    ax.plot(t1, t1, 'bo')


fig = Figure()
graph(fig)

manager = new_figure_manager(1, dpi=72)
manager.canvas.figure = fig
fig.set_canvas(manager.canvas)
fig.savefig("dist/a.svg")
