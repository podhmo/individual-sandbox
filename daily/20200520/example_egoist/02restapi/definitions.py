from app import App
from handofcats import as_subcommand


app = App()


class Article:
    title: str
    content: str


@app.post("/api/articles", metadata={"tags": ["xxx"]})
def create_article(article: Article) -> Article:
    pass


@as_subcommand
def describe() -> None:
    for fn, metadata in app.handlers:
        print(fn, metadata)


@as_subcommand
def emit(*, title: str = "egoist", version: str = "0.0.0") -> None:
    from emit import emit
    from dictknife import loading

    root = emit(app.routes, title=title, version=version)
    loading.dumpfile(root)


as_subcommand.run()
