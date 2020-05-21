import typing as t
from emit import emit
from app import App
from dictknife import loading

app = App()


class Article:
    title: str
    content: str


@app.get("/api/articles", metadata={"tags": ["xxx"]})
def list_article() -> t.List[Article]:
    pass


title = "egoist"
version = "0.0.0"
root = emit(list(app.routes), title=title, version=version)
loading.dumpfile(root)
