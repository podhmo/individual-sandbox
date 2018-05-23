from apistar import App, Route


def hello() -> dict:
    return {"hello": "world"}


routes = [
    Route("/", "GET", hello),
    Route("/", "GET", hello, name="x"),
]

app = App(routes=routes)
app.serve('127.0.0.1', 5000, debug=True)
