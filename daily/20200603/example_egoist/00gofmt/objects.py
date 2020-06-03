from __future__ import annotations
import typing as t


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
