from typing import List, Optional


class Openapimini:
    openapi: str
    info: "Info"
    servers: List["Server"]
    tags: List["Tag"]


class Info:
    description: str
    license: "License"
    title: str
    version: str


class License:
    name: str
    url: str


class Server:
    description: str
    url: str
    variables: "Variable"


class Variable:
    version: Optional["Version"]
    server: Optional["Version"]
    port: Optional["Version"]


class Tag:
    description: str
    name: str


class Version:
    default: str
    enum: List[str]
