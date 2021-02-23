from __future__ import annotations
import typing


class Attachment:
    """
    An object representing a container instance or task attachment.
    """

    details: typing.Optional[typing.List[KeyValuePair]]
    id: typing.Optional[str]
    status: typing.Optional[str]
    type: typing.Optional[str]


class KeyValuePair:
    """
    A key-value pair object.
    """

    name: typing.Optional[str]
    value: typing.Optional[str]
