# -*- coding:utf-8 -*-
import logging
from collections import UserDict
from datetime import datetime
logger = logging.getLogger(__name__)


class Post(UserDict):
    members = ["author", "text", "tags", "date"]
    required_members = ["author", "text", "tags", "date"]
    optional_members = []

    def __init__(self, *args, **kwargs):
        if args:
            # with dict
            super().__init__(args[0])
        else:
            super().__init__(**kwargs)


if __name__ == "__main__":
    post = Post(author="Mike",
                text="My first blog post",
                tags=["mongodb", "python", "pymongo"],
                date=datetime.now())
    post2 = Post(dict(author="Mike",
                      text="My first blog post",
                      tags=["mongodb", "python", "pymongo"],
                      date=datetime.now()))
