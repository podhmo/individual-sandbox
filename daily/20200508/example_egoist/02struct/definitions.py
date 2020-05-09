from __future__ import annotations
import typing as t
from egoist.app import App, SettingsDict

settings: SettingsDict = {"root": "", "here": __file__}
app = App(settings)

app.include("egoist.directives.define_struct_set")


class Author:
    name: str
    # createdAt: datetime.date


class Article:
    title: str
    author: t.Optional[Author]
    content: str
    comments: t.List[Comment]


class Comment:
    author: Author
    content: str


@app.define_struct_set("egoist.generate.structkit:walk")
def model__objects() -> None:
    from egoist.generate.structkit import runtime, structkit

    with runtime.generate(structkit, classes=[Article]) as m:
        m.package("model")


if __name__ == "__main__":
    app.run()
