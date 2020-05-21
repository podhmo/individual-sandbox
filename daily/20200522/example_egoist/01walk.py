import typing as t
from _walk import get_walker


class Article:
    pass


def list_article() -> t.List[Article]:
    pass


for cls in get_walker([list_article]).walk():
    print(cls)
