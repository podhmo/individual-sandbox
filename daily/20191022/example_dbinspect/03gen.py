# -*- coding:utf-8 -*-
import logging
import sqlalchemy as sa
from sqlalchemy.ext.declarative import declarative_base


logger = logging.getLogger(__name__)
Base = declarative_base()

url = "mysql+pymysql://root@127.0.0.1:43306/sakila"
engine = sa.create_engine(url, echo=False)


class db:
    tables = []


logging.basicConfig(level=logging.INFO)
for name in engine.table_names():
    # logger.info("loading .. table name: %s", name)
    table = sa.Table(name, Base.metadata, autoload=True, autoload_with=engine)
    setattr(db, name, table)
    db.tables.append(table)

from prestring.python import PythonModule

m = PythonModule()


def captilize(s):
    return "{}{}".format(s[0].upper(), s[1:].lower())


def usually_name_of_type(t):
    name = t.__name__
    if name.upper() == name and name.lower() == t.__mro__[1].__name__.lower():
        return t.__mro__[1].__name__
    else:
        return name


def gen_class(m, table, prefix={"sa": "sa", "orm": "orm", "Base": "Base"}):
    with m.class_(captilize(table.name), prefix["Base"]):
        m.stmt("__tablename__ = {!r}".format(table.name))
        m.sep()

        for col in table.columns:
            m.append(
                "{name} = {prefix[sa]}.Column(".format(name=col.name, prefix=prefix)
            )
            m.append(
                "{prefix[sa]}.{type}".format(
                    type=usually_name_of_type(type(col.type)), prefix=prefix
                )
            )
            if getattr(col.type, "length", None) is not None:
                m.append("({})".format(col.type.length))

            if col.foreign_keys:
                if len(col.foreign_keys) > 1:
                    raise NotImplementedError("sorry")
                else:
                    m.append(
                        ", {prefix[sa]}.{k}".format(
                            prefix=prefix, k=list(col.foreign_keys)[0]
                        )
                    )

            if col.primary_key:
                m.append(", primary_key=True")
            if col.index:
                m.append(", index=True")
            if col.unique:
                m.append(", unique=True")
            if col.autoincrement and col.primary_key:
                m.append(", autoincrement=True")
            m.append(", nullable={}".format(col.nullable))
            m.append(")")
            m.sep()

    for c in table.constraints:
        if isinstance(c, sa.ForeignKeyConstraint):
            continue
        if isinstance(c, sa.PrimaryKeyConstraint):
            continue
        print(c)


for table in db.tables:
    gen_class(m, table)

print(
    """
import sqlalchemy as sa
from sqlalchemy.ext.declarative import declarative_base
Base = declarative_base()
"""
)
print(str(m))
