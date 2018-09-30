from dictknife import loading
import typing as t
from schema import Namespace, Object


class Person(Object):
    """person"""

    name: str
    age: int


class PersonWithNickname(Person):
    """person (with nickname)"""
    nickname: t.Optional[str] = "str"


with Namespace("components") as components:
    with Namespace("schemas", ns=components) as schemas:
        schemas.mount(PersonWithNickname)
    loading.dumpfile(components.as_dict(), format="json")
