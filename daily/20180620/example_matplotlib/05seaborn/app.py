from io import BytesIO
from apistar import App, Route
from apistar import http
import matplotlib as mlp
mlp.use("agg")  # noqa
import seaborn as sns
sns.set()


def download():
    # Load the example tips dataset
    iris = sns.load_dataset("iris")

    # Plot tip as a function of toal bill across days
    g = sns.lmplot(
        x="sepal_length", y="sepal_width", hue="species", truncate=True, size=5, data=iris
    )

    # Use more informative axis labels than are provided by default
    g.set_axis_labels("Sepal length (mm)", "Sepal width (mm)")

    fig = g.fig
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
