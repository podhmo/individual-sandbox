import typing as t
from handofcats.accessor import Accessor


def hello(name: str, nickname: t.Optional[str] = None):
    pass


def hello2(*, name: str, nickname: t.Optional[str] = None):
    pass


a = Accessor(hello)
print("arguments", a.arguments)
print("flags", a.flags)

# [Option(name='name', option_name='name', required=True, type=<class 'str'>, default=None)]
# [Option(name='nickname', option_name='--nickname', required=False, type=typing.Union[str, NoneType], default=None)]
print("----------------------------------------")

a = Accessor(hello2)
print("arguments", a.arguments)
print("flags", a.flags)
