from io import BytesIO
from apistar import App, Route
from apistar import http
import numpy as np
import matplotlib as mlp
mlp.use("agg")  # noqa
import matplotlib.pyplot as plt


def download():
    fig = plt.figure(1)
    ax = fig.add_subplot(111)
    t = np.arange(0.0, 3.0, 0.01)
    s = np.sin(2 * np.pi * t)
    ax.plot(t, s)

    buf = BytesIO()
    fig.canvas.print_figure(buf, format='svg')

    headers = {
        'Content-Type': 'image/svg+xml',
        'Content-Disposition': 'attachment; filename="graph.svg"',
    }
    return http.Response(buf.getvalue(), headers=headers)


routes = [
    Route('/', method='GET', handler=download),
]

app = App(routes=routes)

if __name__ == '__main__':
    app.serve('127.0.0.1', 5000, debug=True)
