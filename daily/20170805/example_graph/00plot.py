import numpy as np
from utatane import as_command, yield_fixture


@yield_fixture
def data():
    yield np.linspace(-np.pi, np.pi)


@as_command
def render(plt, x):

    plt.plot(x, np.cos(x), color='r', ls='-', label='cos')
    plt.plot(x, np.sin(x), color='b', ls='-', label='sin')
    plt.plot(x, np.tan(x), color='c', marker='s', ls='None', label='tan')

    plt.xlim(-np.pi, np.pi)
    plt.ylim(-1.5, 1.5)

    plt.axhline(0, ls='-', c='b', lw=0.5)
    plt.axvline(0, ls='-', c='b', lw=0.5)

    plt.xlabel('x')
    plt.ylabel('y')
    plt.title('Graphs')
