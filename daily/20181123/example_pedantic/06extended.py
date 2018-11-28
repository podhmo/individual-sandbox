import uuid
import os.path
from decimal import Decimal
from pathlib import Path
from uuid import UUID

from pydantic import (
    DSN,
    UUID1,
    UUID3,
    UUID4,
    UUID5,
    BaseModel,
    DirectoryPath,
    EmailStr,
    FilePath,
    NameEmail,
    NegativeFloat,
    NegativeInt,
    PositiveFloat,
    PositiveInt,
    PyObject,
    UrlStr,
    condecimal,
    confloat,
    conint,
    constr,
)


class Model(BaseModel):
    cos_function: PyObject = None

    path_to_something: Path = None
    path_to_file: FilePath = None
    path_to_directory: DirectoryPath = None

    short_str: constr(min_length=2, max_length=10) = None
    regex_str: constr(regex='apple (pie|tart|sandwich)') = None
    strip_str: constr(strip_whitespace=True)

    big_int: conint(gt=1000, lt=1024) = None
    pos_int: PositiveInt = None
    neg_int: NegativeInt = None

    big_float: confloat(gt=1000, lt=1024) = None
    unit_interval: confloat(ge=0, le=1) = None
    pos_float: PositiveFloat = None
    neg_float: NegativeFloat = None

    email_address: EmailStr = None
    email_and_name: NameEmail = None

    url: UrlStr = None

    db_name = 'foobar'
    db_user = 'postgres'
    db_password: str = None
    db_host = 'localhost'
    db_port = '5432'
    db_driver = 'postgres'
    db_query: dict = None
    dsn: DSN = None
    decimal: Decimal = None
    decimal_positive: condecimal(gt=0) = None
    decimal_negative: condecimal(lt=0) = None
    decimal_max_digits_and_places: condecimal(max_digits=2, decimal_places=2) = None
    uuid_any: UUID = None
    uuid_v1: UUID1 = None
    uuid_v3: UUID3 = None
    uuid_v4: UUID4 = None
    uuid_v5: UUID5 = None


m = Model(
    cos_function='math.cos',
    path_to_something='/home',
    path_to_file=os.path.expanduser('~/.pypirc'),
    path_to_directory=os.path.expanduser('~/.config'),
    short_str='foo',
    regex_str='apple pie',
    strip_str='   bar',
    big_int=1001,
    pos_int=1,
    neg_int=-1,
    big_float=1002.1,
    pos_float=2.2,
    neg_float=-2.3,
    unit_interval=0.5,
    email_address='Samuel Colvin <s@muelcolvin.com >',
    email_and_name='Samuel Colvin <s@muelcolvin.com >',
    url='http://example.com',
    decimal=Decimal('42.24'),
    decimal_positive=Decimal('21.12'),
    decimal_negative=Decimal('-21.12'),
    decimal_max_digits_and_places=Decimal('0.99'),
    uuid_any=uuid.uuid4(),
    uuid_v1=uuid.uuid1(),
    uuid_v3=uuid.uuid3(uuid.NAMESPACE_DNS, 'python.org'),
    uuid_v4=uuid.uuid4(),
    uuid_v5=uuid.uuid5(uuid.NAMESPACE_DNS, 'python.org')
)
print(m.dict())
# -- stdout --------------------
# >> {'strip_str': 'bar', 'cos_function': <built-in function cos>, 'path_to_something': PosixPath('/home'), 'path_to_file': PosixPath('/home/nao/.pypirc'), 'path_to_directory': PosixPath('/home/nao/.config'), 'short_str': 'foo', 'regex_str': 'apple pie', 'big_int': 1001, 'pos_int': 1, 'neg_int': -1, 'big_float': 1002.1, 'unit_interval': 0.5, 'pos_float': 2.2, 'neg_float': -2.3, 'email_address': 's@muelcolvin.com', 'email_and_name': <NameEmail("Samuel Colvin <s@muelcolvin.com>")>, 'url': 'http://example.com', 'db_name': 'foobar', 'db_user': 'postgres', 'db_password': None, 'db_host': 'localhost', 'db_port': '5432', 'db_driver': 'postgres', 'db_query': None, 'dsn': 'postgres://postgres@localhost:5432/foobar', 'decimal': Decimal('42.24'), 'decimal_positive': Decimal('21.12'), 'decimal_negative': Decimal('-21.12'), 'decimal_max_digits_and_places': Decimal('0.99'), 'uuid_any': UUID('9bd8f882-f074-453c-a881-5ba49390ec96'), 'uuid_v1': UUID('13136674-ef2a-11e8-8b1b-a0c5890bfbcb'), 'uuid_v3': UUID('6fa459ea-ee8a-3ca4-894e-db77e160355e'), 'uuid_v4': UUID('54e9af02-a9e4-47ea-8fde-8c8b9ec3f759'), 'uuid_v5': UUID('886313e1-3b8a-5372-9b90-0c9aee199e5d')}
