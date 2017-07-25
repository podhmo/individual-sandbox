from utatane import as_command


@as_command
def main(plt):
    xs = list(range(1, 11))
    ys = list(map(lambda x: x * x, xs))
    plt.plot(xs, ys)
