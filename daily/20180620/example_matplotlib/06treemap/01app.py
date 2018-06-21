from io import BytesIO
from apistar import App, Route
from apistar import http
import matplotlib as mlp
mlp.use("agg")  # noqa
import matplotlib.pyplot as plt


def download():
    import squarify

    values = sorted([i ** 3 for i in range(1, 50)], reverse=True)

    cmap = mlp.cm.Spectral
    norm = mlp.colors.Normalize(vmin=min(values), vmax=max(values))
    colors = [cmap(norm(value)) for value in values]

    fig = plt.figure(1)
    ax = fig.add_subplot(111)
    squarify.plot(sizes=values, alpha=.8, color=colors, ax=ax)
    ax.axis("off")

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
