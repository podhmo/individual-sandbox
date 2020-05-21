from _walk import collect_types


class Article:
    pass


def create_article(article: Article) -> Article:
    pass


fns = [create_article]
for cls in collect_types(fns):
    print(cls)
