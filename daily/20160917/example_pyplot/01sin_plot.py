# -*- coding:utf-8 -*-
import numpy as np
from matplotlib import pyplot


def pause_plot():
    fig, ax = pyplot.subplots(1, 1)
    x = np.arange(-np.pi, np.pi, 0.1)
    y = np.sin(x)
    z = np.cos(x)
    lines, = ax.plot(x, y)
    lines2, = ax.plot(x, z)

    while True:
        x += 0.1
        y = np.sin(x)
        z = np.cos(x)

        lines.set_data(x, y)
        lines2.set_data(x, z)

        ax.set_xlim((x.min(), x.max()))
        pyplot.pause(.1)


if __name__ == "__main__":
    pause_plot()
