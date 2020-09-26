import typing as t

string = str
bool = bool

Pointer = t.TypeVar("Pointer")

Context = t.NewType("context.Context", object)
error = t.NewType("error", object)
