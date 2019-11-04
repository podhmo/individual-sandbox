from __future__ import annotations
import typing as t


# self references
class Person:
    name: str
    spouse: t.Optional[Person]  # reverse: spouse,  synmetric one-to-one
    friends: t.Set[Person]  # reverse: friends, synmetric many-to-many
    manager: t.Optional[Person]  # reverse: employees, one side of non-synmetric
    employees: t.Set[Person]  # reverse: manager, another side of non-synmetric


# multiple relationships between two entities
class User:
    name: str
    tweets: t.Set[Tweet]  # reverse: author
    favorites: t.Set[Tweet]  # reverse: favorited


class Tweet:
    text: str
    author: User  # reverse tweets
    favorited: t.Set[User]  # reverse favorites


import sys
from metashape.runtime import get_walker
from metashape.outputs.openapi import emit

w = get_walker([User, Person], aggressive=True, recursive=True)
emit(w, output=sys.stdout)
