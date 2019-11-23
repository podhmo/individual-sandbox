import typing as t
import inspect
from typing import ForwardRef


# from fastAPI
def get_typed_signature(call: t.Callable) -> inspect.Signature:
    signature = inspect.signature(call)
    globalns = getattr(call, "__globals__", {})
    typed_params = [
        inspect.Parameter(
            name=param.name,
            kind=param.kind,
            default=param.default,
            annotation=get_typed_annotation(param, globalns),
        )
        for param in signature.parameters.values()
    ]
    typed_signature = inspect.Signature(typed_params)
    return typed_signature


def get_typed_annotation(
    param: inspect.Parameter, globalns: t.Dict[str, t.Any]
) -> t.Any:
    annotation = param.annotation
    if isinstance(annotation, str):
        annotation = ForwardRef(annotation)
        annotation = evaluate_forwardref(annotation, globalns, globalns)
    return annotation


def evaluate_forwardref(type_, globalns, localns):  # type: ignore
    return type_._evaluate(globalns, localns)
