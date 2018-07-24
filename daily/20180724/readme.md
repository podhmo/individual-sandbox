## git submodule込でcloneしたい場合

```
git clone --recurse-submodues <repo>
```

## mypy hmm

- http://mypy.readthedocs.io/en/latest/common_issues.html#covariant-subtyping-of-mutable-protocol-members-is-rejected
- https://github.com/python/mypy/issues/4395
- https://github.com/python/mypy/issues/4395
- https://www.python.org/dev/peps/pep-0544/

Protocolでabstractmethod必要？

- 普通の言語でinterfaceを実装するのとおんなじようなチェックが効く
- https://www.python.org/dev/peps/pep-0544/#explicitly-declaring-implementation

mergeの話

- https://www.python.org/dev/peps/pep-0544/#merging-and-extending-protocols

## mypyの実行パス

とりあえずmypy.main:mainで始まって、mypy.build:dispatchあたりで色々呼ばれる？

たぶんparser的なものが呼ばれるのは、mypy.build:BuildManager.parse_fileのあたり。これはmypy.fastparse:parse。さらにtypes_ast.parse

mypy.fastparse.TypeConverterとmypy.fastparse.ASTConverterが何をやっているのかわかんないな


## python mypy.memprofile とか覗いてみても良いかもしれない

## python mypy EA[EB[T]]

```
(Pdb) pp vars(info)
{'_fullname': '06wip.EY',
 '_promote': None,
 'abstract_attributes': [],
 'assuming': [],
 'assuming_proper': [],
 'bases': [builtins.object],
 'defn': <mypy.nodes.ClassDef object at 0x7fcebf9ab288>,
 'fallback_to_any': False,
 'inferring': [],
 'is_abstract': False,
 'is_protocol': False,
 'metaclass_type': None,
 'metadata': {},
 'module_name': '06wip',
 'mro': [<TypeInfo 06wip.EY>, <TypeInfo builtins.object>],
 'names': {'y': <mypy.nodes.SymbolTableNode object at 0x7fcebeb881c8>},
 'runtime_protocol': False,
 'type_vars': ['T']}
```

```
06wip.EY[06wip.EX*[06wip.Ob@28*]]
(Pdb) original_type.names
*** AttributeError: 'Instance' object has no attribute 'names'
(Pdb) original_type.type
<TypeInfo 06wip.EY>
(Pdb) type(original_type.type)
<class 'mypy.nodes.TypeInfo'>
(Pdb) original_type.type.names
{'y': <mypy.nodes.SymbolTableNode object at 0x7fcebeb881c8>}
```

```python
class ExpressionChecker(ExpressionVisitor[Type]):
    def visit_name_expr(self, e: NameExpr) -> Type:
        """Type check a name expression.

        It can be of any kind: local, member or global.
        """
        self.chk.module_refs.update(extract_refexpr_names(e))
        result = self.analyze_ref_expr(e)
        return self.narrow_type_from_binder(e, result)

    def analyze_ref_expr(self, e: RefExpr, lvalue: bool = False) -> Type:
        result = None  # type: Optional[Type]
        node = e.node
        if isinstance(node, Var):  # <-
            # Variable reference.
            result = self.analyze_var_ref(node, e)
            if isinstance(result, PartialType):
                in_scope, partial_types = self.chk.find_partial_types_in_all_scopes(node)
                if result.type is None and in_scope:
                    # 'None' partial type. It has a well-defined type. In an lvalue context
                    # we want to preserve the knowledge of it being a partial type.
                    if not lvalue:
                        result = NoneTyp()
                else:
                    if partial_types is not None and not self.chk.current_node_deferred:
                        if in_scope:
                            context = partial_types[node]
                            self.msg.need_annotation_for_var(node, context)
                        else:
                            # Defer the node -- we might get a better type in the outer scope
                            self.chk.handle_cannot_determine_type(node.name(), e)
                    result = AnyType(TypeOfAny.special_form)
        elif isinstance(node, FuncDef):
            # Reference to a global function.
            result = function_type(node, self.named_type('builtins.function'))
        elif isinstance(node, OverloadedFuncDef) and node.type is not None:
            # node.type is None when there are multiple definitions of a function
            # and it's decorated by something that is not typing.overload
            result = node.type
        elif isinstance(node, TypeInfo):
            # Reference to a type object.
            result = type_object_type(node, self.named_type)
            if isinstance(result, CallableType) and isinstance(result.ret_type, Instance):
                # We need to set correct line and column
                # TODO: always do this in type_object_type by passing the original context
                result.ret_type.line = e.line
                result.ret_type.column = e.column
            if isinstance(self.type_context[-1], TypeType):
                # This is the type in a Type[] expression, so substitute type
                # variables with Any.an
                result = erasetype.erase_typevars(result)
        elif isinstance(node, MypyFile):
            # Reference to a module object.
            try:
                result = self.named_type('types.ModuleType')
            except KeyError:
                # In test cases might 'types' may not be available.
                # Fall back to a dummy 'object' type instead to
                # avoid a crash.
                result = self.named_type('builtins.object')
        elif isinstance(node, Decorator):
            result = self.analyze_var_ref(node.var, e)
        elif isinstance(node, TypeAlias):
            # Something that refers to a type alias appears in runtime context.
            # Note that we suppress bogus errors for alias redefinitions,
            # they are already reported in semanal.py.
            result = self.alias_type_in_runtime_context(node.target, node.alias_tvars,
                                                        node.no_args, e,
                                                        alias_definition=e.is_alias_rvalue
                                                        or lvalue)
        else:
            # Unknown reference; use any type implicitly to avoid
            # generating extra type errors.
            result = AnyType(TypeOfAny.from_error)
        assert result is not None
        return result



# e.expr
# [<class 'mypy.nodes.NameExpr'>, <class 'mypy.nodes.RefExpr'>, <class 'mypy.nodes.Expression'>, <class 'mypy.nodes.Node'>, <class 'mypy.nodes.Context'>, <class 'object'>]
# e.expr.node
# <mypy.nodes.Var object at 0x7fcebebeae08>

# self.analyze_var_ref(e.expr.node, e.expr)
```

```
(Pdb) e.expr.node.type
06wip.EY[06wip.EX*[06wip.Ob@28*]]
(Pdb) type(e.expr.node.type)
<class 'mypy.types.Instance'>

(Pdb) e.expr.node.type.type
<TypeInfo 06wip.EY>
(Pdb) type(e.expr.node.type.type)
<class 'mypy.nodes.TypeInfo'>
```

## python mypy dmypy

daemon的な機能の挙動

## python mypy

cheat sheet

- http://mypy.readthedocs.io/en/latest/cheat_sheet_py3.html

Protocol

```
$ pip install typing-extensions mypy
```


## python jupyterlab

- https://github.com/jupyterlab/jupyterlab

```console
$ pip install jupyterlab
$ jupyter lab
```



## python ipynbのpreviewを手軽に

- https://github.com/uetchy/juno

## go pointerがキーのmap
