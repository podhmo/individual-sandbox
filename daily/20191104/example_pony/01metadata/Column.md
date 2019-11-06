```console
$ pyinspect inspect sqlalchemy.sql.schema:Table
sqlalchemy.sql.schema:Column <- sqlalchemy.sql.schema:SchemaItem <- sqlalchemy.sql.base:SchemaEventTarget <- sqlalchemy.sql.elements:ColumnClause <- sqlalchemy.sql.base:Immutable <- sqlalchemy.sql.elements:ColumnElement <- sqlalchemy.sql.operators:ColumnOperators <- sqlalchemy.sql.operators:Operators <- sqlalchemy.sql.elements:ClauseElement <- sqlalchemy.sql.visitors:Visitable <- builtins:object
    [method, OVERRIDE] __init__(self, *args, **kwargs)
    [method, OVERRIDE] __repr__(self)
    [method, OVERRIDE] __str__(self)
    [method, OVERRIDE] _compiler_dispatch(self, visitor, **kw)
    [method, OVERRIDE] _make_proxy(self, selectable, name=None, key=None, name_is_truncatable=False, **kw)
    [method] _on_table_attach(self, fn)
    [method, OVERRIDE] _set_parent(self, table)
        [method] _setup_on_memoized_fks(self, fn)
    [method] append_foreign_key(self, fk)
    [method] copy(self, **kw)
    [method, OVERRIDE] get_children(self, schema_visitor=False, **kwargs)
    [method] references(self, column)

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

sqlalchemy.sql.elements:ColumnClause <- sqlalchemy.sql.base:Immutable <- sqlalchemy.sql.elements:ColumnElement <- sqlalchemy.sql.operators:ColumnOperators <- sqlalchemy.sql.operators:Operators <- sqlalchemy.sql.elements:ClauseElement <- sqlalchemy.sql.visitors:Visitable <- builtins:object
    [method, OVERRIDE] __init__(self, text, type_=None, is_literal=False, _selectable=None)
        [property] table
    [method, OVERRIDE] _bind_param(self, operator, obj, type_=None)
    [method, OVERRIDE] _compare_name_for_result(self, other)
        [property] table
        [method, OVERRIDE] _label
    [method, OVERRIDE] _compiler_dispatch(self, visitor, **kw)
    [method, OVERRIDE] _from_objects
    [method] _gen_label(self, name)
        [property] table
    [method] _get_table(self)
    [method, OVERRIDE] _key_label
    [method, OVERRIDE] _make_proxy(self, selectable, name=None, attach=True, name_is_truncatable=False, **kw)
    [method, OVERRIDE] _render_label_in_columns_clause
    [method] _set_table(self, table)
    [method, OVERRIDE] description

sqlalchemy.sql.base:Immutable <- builtins:object
    [method] _clone(self)
    [method] params(self, *optionaldict, **kwargs)
    [method] unique_params(self, *optionaldict, **kwargs)

sqlalchemy.sql.elements:ColumnElement <- sqlalchemy.sql.operators:ColumnOperators <- sqlalchemy.sql.operators:Operators <- sqlalchemy.sql.elements:ClauseElement <- sqlalchemy.sql.visitors:Visitable <- builtins:object
    [method] __getattr__(self, key)
        [method] comparator
    [method] _bind_param(self, operator, obj, type_=None)
        [method] type
    [method] _compare_name_for_result(self, other)
    [method, OVERRIDE] _compiler_dispatch(self, visitor, **kw)
    [method] _make_proxy(self, selectable, name=None, name_is_truncatable=False, **kw)
        [method] anon_label
    [method, OVERRIDE] _negate(self)
        [method] type
    [property] _select_iterable
    [method] base_columns
    [method] cast(self, type_)
    [method, OVERRIDE] compare(self, other, use_proxies=False, equivalents=None, **kw)
        [method] shares_lineage(self, othercolumn)
            [method] proxy_set
    [property] expression
    [method] label(self, name)
        [method] type
    [method, OVERRIDE] operate(self, op, *other, **kwargs)
        [method] comparator
    [method, OVERRIDE] reverse_operate(self, op, other, **kwargs)
        [method] comparator
    [method, OVERRIDE] self_group(self, against=None)
        [method] type

sqlalchemy.sql.operators:ColumnOperators <- sqlalchemy.sql.operators:Operators <- builtins:object
    [method] __add__(self, other)
    [method] __contains__(self, other)
    [method] __div__(self, other)
    [method, OVERRIDE] __eq__(self, other)
    [method, OVERRIDE] __ge__(self, other)
    [method] __getitem__(self, index)
    [method, OVERRIDE] __gt__(self, other)
    [method, OVERRIDE] __hash__(self, /)
    [method, OVERRIDE] __le__(self, other)
    [method] __lshift__(self, other)
    [method, OVERRIDE] __lt__(self, other)
    [method] __mod__(self, other)
    [method] __mul__(self, other)
    [method, OVERRIDE] __ne__(self, other)
    [method] __neg__(self)
    [method] __radd__(self, other)
    [method] __rdiv__(self, other)
    [method] __rmod__(self, other)
    [method] __rmul__(self, other)
    [method] __rshift__(self, other)
    [method] __rsub__(self, other)
    [method] __rtruediv__(self, other)
    [method] __sub__(self, other)
    [method] __truediv__(self, other)
    [method] all_(self)
    [method] any_(self)
    [method] asc(self)
    [method] between(self, cleft, cright, symmetric=False)
    [method] collate(self, collation)
    [method] concat(self, other)
    [method] contains(self, other, **kwargs)
    [method] desc(self)
    [method] distinct(self)
    [method] endswith(self, other, **kwargs)
    [method] ilike(self, other, escape=None)
    [method] in_(self, other)
    [method] is_(self, other)
    [method] is_distinct_from(self, other)
    [method] isnot(self, other)
    [method] isnot_distinct_from(self, other)
    [method] like(self, other, escape=None)
    [method] match(self, other, **kwargs)
    [method] notilike(self, other, escape=None)
    [method] notin_(self, other)
    [method] notlike(self, other, escape=None)
    [method] nullsfirst(self)
    [method] nullslast(self)
    [method] startswith(self, other, **kwargs)

sqlalchemy.sql.operators:Operators <- builtins:object
    [method] __and__(self, other)
        [method] operate(self, op, *other, **kwargs)
    [method] __invert__(self)
        [method] operate(self, op, *other, **kwargs)
    [method] __or__(self, other)
        [method] operate(self, op, *other, **kwargs)
    [method] op(self, opstring, precedence=0, is_comparison=False)
    [method] reverse_operate(self, op, other, **kwargs)

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
    [method] _copy_internals(self, clone=<function ClauseElement._clone at 0x7ff4a6223dd0>, **kw)
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
