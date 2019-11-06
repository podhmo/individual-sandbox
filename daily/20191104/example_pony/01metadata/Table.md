```console
$ pyinspect inspect sqlalchemy.sql.schema:Table
sqlalchemy.sql.schema:Table <- sqlalchemy.sql.base:DialectKWArgs <- sqlalchemy.sql.schema:SchemaItem <- sqlalchemy.sql.base:SchemaEventTarget <- sqlalchemy.sql.selectable:TableClause <- sqlalchemy.sql.base:Immutable <- sqlalchemy.sql.selectable:FromClause <- sqlalchemy.sql.selectable:Selectable <- sqlalchemy.sql.elements:ClauseElement <- sqlalchemy.sql.visitors:Visitable <- builtins:object
    [method, OVERRIDE] __init__(self, *args, **kw)
    [static method, OVERRIDE] __new__(cls, *args, **kw)
    [method, OVERRIDE] __repr__(self)
    [method, OVERRIDE] __str__(self)
    [property, OVERRIDE] _autoincrement_column
    [method, OVERRIDE] _compiler_dispatch(self, visitor, **kw)
    [method] _init(self, name, metadata, *args, **kwargs)
        [method] _extra_kwargs(self, **kwargs)
        [method] _autoload(self, metadata, autoload_with, include_columns, exclude_columns=(), _extend_on=None)
    [method, OVERRIDE] _init_collections(self)
    [method] _init_existing(self, *args, **kwargs)
        [method] _extra_kwargs(self, **kwargs)
        [method] _autoload(self, metadata, autoload_with, include_columns, exclude_columns=(), _extend_on=None)
    [method, OVERRIDE] _reset_exported(self)
    [method, OVERRIDE] _set_parent(self, metadata)
    [property] _sorted_constraints
    [method] add_is_dependent_on(self, table)
    [method, OVERRIDE] append_column(self, column)
    [method] append_constraint(self, constraint)
    [method] append_ddl_listener(self, event_name, listener)
    [property, OVERRIDE] bind
    [method] create(self, bind=None, checkfirst=False)
    [method] drop(self, bind=None, checkfirst=False)
    [method] exists(self, bind=None)
    [property] foreign_key_constraints
    [method, OVERRIDE] get_children(self, column_collections=True, schema_visitor=False, **kw)
    [property] key
    [property] quote_schema
    [method] tometadata(self, metadata, schema=symbol('retain_schema'), referred_schema_fn=None, name=None)

sqlalchemy.sql.base:DialectKWArgs <- builtins:object
    [method] _kw_reg_for_dialect(dialects, dialect_name)
    [method] _kw_reg_for_dialect_cls(self, dialect_name)
    [method] _validate_dialect_kwargs(self, kwargs)
        [method] dialect_options
    [class method] argument_for(dialect_name, argument_name, default)
    [method] dialect_kwargs
    [property] kwargs

sqlalchemy.sql.schema:SchemaItem <- sqlalchemy.sql.base:SchemaEventTarget <- sqlalchemy.sql.visitors:Visitable <- builtins:object
    [method, OVERRIDE] __repr__(self)
    [method] _compiler_dispatch(self, visitor, **kw)
    [method] _init_items(self, *args)
    [method] _schema_item_copy(self, schema_item)
        [method] info
    [method] _translate_schema(self, effective_schema, map_)
    [method] get_children(self, **kwargs)
    [property] quote

sqlalchemy.sql.base:SchemaEventTarget <- builtins:object
    [method] _set_parent_with_dispatch(self, parent)
        [method] _set_parent(self, parent)

sqlalchemy.sql.selectable:TableClause <- sqlalchemy.sql.base:Immutable <- sqlalchemy.sql.selectable:FromClause <- sqlalchemy.sql.selectable:Selectable <- sqlalchemy.sql.elements:ClauseElement <- sqlalchemy.sql.visitors:Visitable <- builtins:object
    [method, OVERRIDE] __init__(self, name, *columns)
        [method] append_column(self, c)
    [method, OVERRIDE] _compiler_dispatch(self, visitor, **kw)
    [property, OVERRIDE] _from_objects
    [method, OVERRIDE] _init_collections(self)
    [method] delete(self, dml, whereclause=None, **kwargs)
    [method, OVERRIDE] description
    [method, OVERRIDE] get_children(self, column_collections=True, **kwargs)
    [method] insert(self, dml, values=None, inline=False, **kwargs)
    [method] update(self, dml, whereclause=None, values=None, inline=False, **kwargs)

sqlalchemy.sql.base:Immutable <- builtins:object
    [method] _clone(self)
    [method] params(self, *optionaldict, **kwargs)
    [method] unique_params(self, *optionaldict, **kwargs)

sqlalchemy.sql.selectable:FromClause <- sqlalchemy.sql.selectable:Selectable <- sqlalchemy.sql.elements:ClauseElement <- sqlalchemy.sql.visitors:Visitable <- builtins:object
    [method, OVERRIDE] _compiler_dispatch(self, visitor, **kw)
    [method] _init_collections(self)
        [method] primary_key
        [method] foreign_keys
    [method] _is_lexical_equivalent(self, other)
    [method] _populate_column_collection(self)
    [method] _refresh_for_new_column(self, column)
        [property] _cols_populated
        [method] columns
    [method] _reset_exported(self)
    [property] _select_iterable
    [method] _translate_schema(self, effective_schema, map_)
    [method] alias(self, name=None, flat=False)
    [method] correspond_on_equivalents(self, column, equivalents)
        [method] corresponding_column(self, column, require_embedded=False)
            [property] c
    [method] count(self, functions, whereclause=None, **params)
        [method] primary_key
        [method] columns
    [property, OVERRIDE] description
    [method] is_derived_from(self, fromclause)
    [method] join(self, right, onclause=None, isouter=False, full=False)
    [method] lateral(self, name=None)
    [method] outerjoin(self, right, onclause=None, full=False)
    [method] replace_selectable(self, sqlutil, old, alias)
    [method] select(self, whereclause=None, **params)
    [method] tablesample(self, sampling, name=None, seed=None)

sqlalchemy.sql.selectable:Selectable <- sqlalchemy.sql.elements:ClauseElement <- sqlalchemy.sql.visitors:Visitable <- builtins:object
    [method, OVERRIDE] _compiler_dispatch(self, visitor, **kw)
    [property] selectable

sqlalchemy.sql.elements:ClauseElement <- sqlalchemy.sql.visitors:Visitable <- builtins:object
    [method] __and__(self, other)
    [method] __bool__(self)
    [method] __getstate__(self)
    [method] __invert__(self)
        [method] _negate(self)
            [method] self_group(self, against=None)
    [method] __nonzero__(self)
    [method] __or__(self, other)
    [method, OVERRIDE] __repr__(self)
    [method, OVERRIDE] __str__(self)
        [method] compile(self, default, bind=None, dialect=None, **kw)
            [method] _compiler(self, dialect, **kw)
    [method] _annotate(self, values)
    [method] _cloned_set
    [method] _compiler_dispatch(self, visitor, **kw)
    [property] _constructor
    [method] _copy_internals(self, clone=<function ClauseElement._clone at 0x7f4fd2a37dd0>, **kw)
    [method] _deannotate(self, values=None, clone=False)
        [method] _clone(self)
    [method] _execute_on_connection(self, connection, multiparams, params)
    [method] _with_annotations(self, values)
    [method] compare(self, other, **kw)
    [method] get_children(self, **kwargs)
    [method] params(self, *optionaldict, **kwargs)
        [method] _params(self, unique, optionaldict, kwargs)
    [method] unique_params(self, *optionaldict, **kwargs)
        [method] _params(self, unique, optionaldict, kwargs)

sqlalchemy.sql.visitors:Visitable <- builtins:object

```
