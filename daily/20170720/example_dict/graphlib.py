import sys
import os
import contextlib


@contextlib.contextmanager
def show():
    import matplotlib.pyplot as plt
    plt.style.use("ggplot")
    yield plt
    plt.legend()
    plt.show()


@contextlib.contextmanager
def save(*, name="fig.svg", w=400, h=300):
    import matplotlib
    matplotlib.use("AGG")  # NOQA
    import matplotlib.pyplot as plt  # NOQA
    plt.style.use("ggplot")
    yield plt
    dpi = float(plt.gcf().get_dpi())
    plt.gcf().set_size_inches(w / dpi, h / dpi)
    print("save:", name, file=sys.stderr)
    plt.savefig(name, dpi=dpi)


@contextlib.contextmanager
def draw(*, name="SAVE", **kwargs):
    filename = os.getenv(name)
    if filename:
        with save(**kwargs) as plt:
            yield plt
    else:
        with show() as plt:
            yield plt
