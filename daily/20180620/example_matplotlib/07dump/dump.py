import numpy as np
from matplotlib.figure import Figure
from matplotlib.backends.backend_svg import new_figure_manager


def f(t):
    return np.exp(-t) * np.cos(2 * np.pi * t)


def graph(fig):
    t1 = np.arange(0.0, 5.0, 0.5)
    ax = fig.add_subplot(1, 1, 1)
    ax.plot(t1, f(t1), 'bo')
    ax.axis("off")


fig = Figure()
graph(fig)

manager = new_figure_manager(1, dpi=72)
manager.canvas.figure = fig
fig.set_canvas(manager.canvas)
fig.savefig("a.svg")
