from __future__ import annotations
import typing as t
from metashape.outputs.openapi import types


class LoginUser:
    email: str
    password: types.password


class LoginUserRequest:
    user: LoginUser


class NewUser:
    username: str
    email: str
    password: types.password


class NewUserRequest:
    user: NewUser


class User:
    email: str
    token: str
    username: str
    bio: str
    image: str


class UserResponse:
    user: User


class UpdateUser:
    email: t.Optional[str]
    token: t.Optional[str]
    username: t.Optional[str]
    bio: t.Optional[str]
    image: t.Optional[str]


class UpdateUserRequest:
    user: UpdateUser


class ProfileResponse:
    profile: Profile


class Profile:
    username: str
    bio: str
    image: str
    following: bool


class Article:
    slug: str
    title: str
    description: str
    body: str
    tagList: t.List[str]
    createdAt: types.date_time
    updatedAt: types.date_time
    favorited: bool
    favoritesCount: int
    author: Profile


class SingleArticleResponse:
    article: Article


class MultipleArticlesResponse:
    articles = t.List[Article]
    articlesCount: int


class NewArticle:
    title: str
    description: str
    body: str
    tagList = t.List[t.Optional[str]]


class NewArticleRequest:
    article: NewArticle


class UpdateArticle:
    title: t.Optional[str]
    description: t.Optional[str]
    body: t.Optional[str]


class UpdateArticleRequest:
    article: UpdateArticle


class Comment:
    id: int
    createdAt: types.date_time
    updatedAt: types.date_time
    body: str
    author: Profile


class SingleCommentResponse:
    comment: Comment


class MultipleCommentsResponse:
    comments = t.List[Comment]


class NewComment:
    body: str


class NewCommentRequest:
    comment: NewComment


class TagsResponse:
    tags: t.List[str]


class GenericErrorModel:
    errors: GenericErrorModelErrors


class GenericErrorModelErrors:
    body: t.List[str]
