import typing as t
from _walk import collect_types


class Article:
    pass


def list_article() -> t.List[Article]:
    pass


fns = [list_article]
for cls in collect_types(fns):
    print(cls)
