from __future__ import annotations
import typing


class Attachment:
    """
    An object representing a container instance or task attachment.
    """

    details: typing.Optional[typing.List[KeyValuePair]]
    id: typing.Optional[AttachmentId]
    status: typing.Optional[AttachmentStatus]
    type: typing.Optional[AttachmentType]


AttachmentId = str  # The unique identifier for the attachment.
AttachmentStatus = str  # The status of the attachment. Valid values are <code>PRECREATED</code>, <code>CREATED</code>, <code>ATTACHING</code>, <code>ATTACHED</code>, <code>DETACHING</code>, <code>DETACHED</code>, and <code>DELETED</code>.
AttachmentType = (
    str  # The type of the attachment, such as <code>ElasticNetworkInterface</code>.
)


class KeyValuePair:
    """
    A key-value pair object.
    """

    name: typing.Optional[
        str
    ]  # The name of the key-value pair. For environment variables, this is the name of the environment variable.
    value: typing.Optional[
        str
    ]  # The value of the key-value pair. For environment variables, this is the value of the environment variable.
