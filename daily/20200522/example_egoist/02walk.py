import typing as t
from _walk import get_walker


class Article:
    pass


def list_article() -> t.Dict[str, t.List[Article]]:
    pass


fns = [list_article]
for cls in collect_types(fns):
    print(cls)
