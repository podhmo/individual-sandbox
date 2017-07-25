from utatane import as_command, yield_fixture


@yield_fixture
def data():
    f = lambda x: x * x  # NOQA
    xs = list(range(1, 11))
    ys = list(map(f, xs))
    yield {"data": [xs, ys]}


@as_command
def main(plt, data):
    plt.plot(data[0], data[1])
