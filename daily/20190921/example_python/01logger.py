from __future__ import annotations
import typing as t
import types
import logging

from typing_extensions import Protocol
from dictknife.langhelpers import reify


logger = logging.getLogger(__name__)


_SysExcInfoType = t.Union[
    t.Tuple[type, BaseException, t.Optional[types.TracebackType]],
    t.Tuple[None, None, None],
]
_ExcInfoType = t.Union[None, bool, _SysExcInfoType, BaseException]


class Logger(Protocol):
    def debug(
        self,
        msg: t.Any,
        *args: t.Any,
        exc_info: _ExcInfoType = ...,
        stack_info: bool = ...,
        extra: t.Optional[t.Dict[str, t.Any]] = ...,
        **kwargs: t.Any
    ) -> None:
        ...

    def info(
        self,
        msg: t.Any,
        *args: t.Any,
        exc_info: _ExcInfoType = ...,
        stack_info: bool = ...,
        extra: t.Optional[t.Dict[str, t.Any]] = ...,
        **kwargs: t.Any
    ) -> None:
        ...

    def warning(
        self,
        msg: t.Any,
        *args: t.Any,
        exc_info: _ExcInfoType = ...,
        stack_info: bool = ...,
        extra: t.Optional[t.Dict[str, t.Any]] = ...,
        **kwargs: t.Any
    ) -> None:
        ...

    def warn(
        self,
        msg: t.Any,
        *args: t.Any,
        exc_info: _ExcInfoType = ...,
        stack_info: bool = ...,
        extra: t.Optional[t.Dict[str, t.Any]] = ...,
        **kwargs: t.Any
    ) -> None:
        ...

    def error(
        self,
        msg: t.Any,
        *args: t.Any,
        exc_info: _ExcInfoType = ...,
        stack_info: bool = ...,
        extra: t.Optional[t.Dict[str, t.Any]] = ...,
        **kwargs: t.Any
    ) -> None:
        ...


class LoggerWithCollectMessage(logging.LoggerAdapter, Logger):
    @reify
    def messages(self) -> t.List[str]:
        return []

    def process(
        self, msg: t.Any, kwargs: t.MutableMapping[str, t.Any]
    ) -> t.Tuple[t.Any, t.MutableMapping[str, t.Any]]:
        self.messages.append(msg)
        return (msg, kwargs)
