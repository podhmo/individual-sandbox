from typing import (
    Dict,
    List,
    Optional,
    Set,
    Tuple,
    Union,
)
from pydantic import BaseModel


class Model(BaseModel):
    simple_list: list = None
    list_of_ints: List[int] = None

    simple_tuple: tuple = None
    tuple_of_different_types: Tuple[int, float, str, bool] = None

    simple_dict: dict = None
    dict_str_float: Dict[str, float] = None

    simple_set: set = None
    set_bytes: Set[bytes] = None

    str_or_bytes: Union[str, bytes] = None
    none_or_str: Optional[str] = None

    computed: Dict[Union[str, bytes], List[Set[int]]] = None


def run(data):
    return Model(**data)


run({"simple_list": ["1", "2", "3"]}).simple_list  # => ['1', '2', '3']
run(dict(simple_list=['1', '2', '3'])).simple_list  # => ['1', '2', '3']
run(dict(list_of_ints=['1', '2', '3'])).list_of_ints  # => [1, 2, 3]

run(dict(simple_dict={'a': 1, b'b': 2})).simple_dict  # => {'a': 1, b'b': 2}
run(dict(dict_str_float={'a': 1, b'b': 2})).dict_str_float  # => {'a': 1.0, 'b': 2.0}

run(dict(simple_tuple=[1, 2, 3, 4])).simple_tuple  # => (1, 2, 3, 4)
run(dict(tuple_of_different_types=[1, 2, 3, 4])).tuple_of_different_types  # => (1, 2.0, '3', True)
