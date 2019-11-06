from __future__ import annotations
import typing as t
from metashape.declarative import field, ORIGINAL_NAME
# from: https://github.com/k1LoW/tbls/blob/master/schema/schema.go


class Index:
    """Index is the struct for database index"""

    name: str
    def_: str = field("", metadata={ORIGINAL_NAME: "def"})
    table: t.Optional[str]
    columns: t.List[str]


class Constraint:
    """Constraint is the struct for database constraint"""

    name: str
    type: str  # type
    def_: str = field("", metadata={ORIGINAL_NAME: "def"})_
    table: t.Optional[str]
    reference_table: t.Optional[str]
    columns: t.List[str]
    reference_columns: t.List[str]


class Trigger:
    """ Trigger is the struct for database trigger"""

    name: str
    def_: str = field("", metadata={ORIGINAL_NAME: "def"})_


class Column:
    """Column is the struct for table column"""

    name: str
    type_: str
    nullable: bool
    default: t.Optional[str]  # sql.NullString
    comment: str
    parent_relations: t.List[Relation]  # -
    child_relations: t.List[Relation]  # -


class Table:
    """Table is the struct for database table"""

    name: str
    type_: str
    comment: str
    columns: t.List[Column]
    indexes: t.List[Index]
    constraints: t.List[Constraint]
    triggers: t.List[Trigger]
    def_: str = field("", metadata={ORIGINAL_NAME: "def"})


class Relation:
    """Relation is the struct for table relation"""

    table: Table
    columns: t.List[Column]
    parent_table: t.Optional[Table]
    parent_columns: t.List[Column]
    def_: str = field("", metadata={ORIGINAL_NAME: "def"})
    is_additional: bool


class Driver:
    """Driver is the struct for tbls driver information"""

    name: str
    database_version: str


class Schema:
    """Schema is the struct for database schema"""

    name: str
    tables: t.List[Table]
    relations: t.List[Relation]
    driver: Driver
