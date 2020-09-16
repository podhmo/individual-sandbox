# from __future__ import annotations
import typing as t
import dataclasses

T = t.TypeVar("T")


@dataclasses.dataclass
class A(t.Generic[T]):
    items: t.List[T]


@dataclasses.dataclass
class B:
    b: int


# note: Revealed type is '00dataclasses.A[00dataclasses.B*]'
a = A(items=[B(b=10)])
print(dataclasses.fields(A))
print(getattr(A, dataclasses._FIELDS))
print("-")

for f in dataclasses.fields(a):
    typ = f.type

    # from __future__ import annotations などを付けているとstrになりそう
    if isinstance(typ, str):
        typ = t.ForwardRef(typ)._evaluate(globalns=globals(), localns=None)

    args = t.get_args(typ)
    if args:
        # specializeされているものに関しても見るのは無駄かも
        actual_args = []
        for arg in args:
            if arg == T:
                actual_args.append(int)  # A[B] に限る
            else:
                # 実際にはList[List]などのことも考えて再帰しないとだめかも
                actual_args.append(arg)

        # 3.9以降はlistをt.Listのように使えるかも
        mapping = {list: t.List}
        typ = mapping[t.get_origin(typ)][tuple(actual_args)]
        # 必ずしもList[int]のような形で型が作れるとは限らないかもしれない

    print(typ)
