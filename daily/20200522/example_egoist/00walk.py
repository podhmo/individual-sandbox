from _walk import get_walker


class Article:
    pass


def create_article(article: Article) -> Article:
    pass


for cls in get_walker([create_article]).walk():
    print(cls)
