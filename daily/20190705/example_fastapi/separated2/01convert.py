import time
import os
from faker import Faker
from typing import List, Any
from pydantic import BaseModel
from enum import Enum
from types import GeneratorType
from typing import Any, List, Set

from pydantic import BaseModel
from pydantic.json import ENCODERS_BY_TYPE


def jsonable_encoder(
    obj: Any,
    include: Set[str] = None,
    exclude: Set[str] = set(),
    by_alias: bool = True,
    skip_defaults: bool = False,
    include_none: bool = True,
    custom_encoder: dict = {},
    sqlalchemy_safe: bool = True,
) -> Any:
    if include is not None and not isinstance(include, set):
        include = set(include)
    if exclude is not None and not isinstance(exclude, set):
        exclude = set(exclude)
    return _jsonable_encoder(
        obj,
        include=include,
        exclude=exclude,
        by_alias=by_alias,
        skip_defaults=skip_defaults,
        include_none=include_none,
        custom_encoder=custom_encoder,
        sqlalchemy_safe=sqlalchemy_safe,
    )


def _jsonable_encoder(
    obj: Any,
    include: Set[str] = None,
    exclude: Set[str] = set(),
    by_alias: bool = True,
    skip_defaults: bool = False,
    include_none: bool = True,
    custom_encoder: dict = {},
    sqlalchemy_safe: bool = True,
) -> Any:
    if isinstance(obj, BaseModel):
        encoder = getattr(obj.Config, "json_encoders", custom_encoder)
        return _jsonable_encoder(
            obj.dict(
                include=include,
                exclude=exclude,
                by_alias=by_alias,
                skip_defaults=skip_defaults,
            ),
            include_none=include_none,
            custom_encoder=encoder,
            sqlalchemy_safe=sqlalchemy_safe,
        )
    if isinstance(obj, Enum):
        return obj.value
    if isinstance(obj, (str, int, float, type(None))):
        return obj
    if isinstance(obj, dict):
        encoded_dict = {}
        for key, value in obj.items():
            if (
                (
                    not sqlalchemy_safe
                    or (not isinstance(key, str))
                    or (not key.startswith("_sa"))
                )
                and (value is not None or include_none)
                and ((include and key in include) or key not in exclude)
            ):
                encoded_key = _jsonable_encoder(
                    key,
                    by_alias=by_alias,
                    skip_defaults=skip_defaults,
                    include_none=include_none,
                    custom_encoder=custom_encoder,
                    sqlalchemy_safe=sqlalchemy_safe,
                )
                encoded_value = _jsonable_encoder(
                    value,
                    by_alias=by_alias,
                    skip_defaults=skip_defaults,
                    include_none=include_none,
                    custom_encoder=custom_encoder,
                    sqlalchemy_safe=sqlalchemy_safe,
                )
                encoded_dict[encoded_key] = encoded_value
        return encoded_dict
    if isinstance(obj, (list, set, frozenset, GeneratorType, tuple)):
        encoded_list = []
        for item in obj:
            encoded_list.append(
                _jsonable_encoder(
                    item,
                    include=include,
                    exclude=exclude,
                    by_alias=by_alias,
                    skip_defaults=skip_defaults,
                    include_none=include_none,
                    custom_encoder=custom_encoder,
                    sqlalchemy_safe=sqlalchemy_safe,
                )
            )
        return encoded_list
    errors: List[Exception] = []
    try:
        if custom_encoder and type(obj) in custom_encoder:
            encoder = custom_encoder[type(obj)]
        else:
            encoder = ENCODERS_BY_TYPE[type(obj)]
        return encoder(obj)
    except KeyError as e:
        errors.append(e)
        try:
            data = dict(obj)
        except Exception as e:
            errors.append(e)
            try:
                data = vars(obj)
            except Exception as e:
                errors.append(e)
                raise ValueError(errors)
    return _jsonable_encoder(
        data,
        by_alias=by_alias,
        skip_defaults=skip_defaults,
        include_none=include_none,
        custom_encoder=custom_encoder,
        sqlalchemy_safe=sqlalchemy_safe,
    )


X = int(os.environ.get("X") or "10000")
fake = Faker()


class BigData(BaseModel):
    key: List[List[Any]]


def route():
    bigdata = {
        "key": [
            [fake.email for i in range(X)],
            [fake.name for i in range(X)],
            [fake.random for i in range(X)],
            [fake.password for i in range(X)],
        ]
    }
    return BigData(**bigdata)


def run():
    st = time.time()
    data = route()
    response = jsonable_encoder(data)
    print("@@", time.time() - st)


run()
run()
run()
