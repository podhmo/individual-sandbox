from utatane import as_command


def example_data(f):
    xs = list(range(1, 11))
    ys = list(map(f, xs))
    return [xs, ys]


@as_command
def main(plt):
    data = example_data(lambda x: x * x)
    plt.plot(data[0], data[1])
