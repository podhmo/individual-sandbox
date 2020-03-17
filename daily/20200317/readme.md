## python graphql-core-next

そういえばresolverの見つけ方の情報をまとめておきたいかも。

https://github.com/graphql-python/graphql-core

記憶が確かなら、`graphql_sync()`あたりからみていけば良い。

```
    graphql_sync(schema: graphql.type.schema.GraphQLSchema, source: Union[str, graphql.language.source.Source], root_value: Any = None, context_value: Any = None, variable_values: Union[Dict[str, Any], NoneType] = None, operation_name: Union[str, NoneType] = None, field_resolver: Union[Callable[..., Any], NoneType] = None, type_resolver: Union[Callable[[Any, graphql.type.definition.GraphQLResolveInfo, ForwardRef('GraphQLAbstractType')], Union[Awaitable[Union[ForwardRef('GraphQLObjectType'), str, NoneType]], ForwardRef('GraphQLObjectType'), str, NoneType]], NoneType] = None, middleware: Union[Tuple, List, graphql.execution.middleware.MiddlewareManager, NoneType] = None, execution_context_class: Type[graphql.execution.execute.ExecutionContext] = <class 'graphql.execution.execute.ExecutionContext'>) -> graphql.execution.execute.ExecutionResult
        Execute a GraphQL operation synchronously.

        The graphql_sync function also fulfills GraphQL operations by parsing, validating,
        and executing a GraphQL document along side a GraphQL schema. However, it guarantees
        to complete synchronously (or throw an error) assuming that all field resolvers
        are also synchronous.
```

気にするのは`root_value` 部分。

内部的には`graphql_impl()`を呼んでいて、`graphql.execution.execute.execute()`色々呼び出された後に``に行き着く

- execution_context_class -> ExecutionContextがdefault
- ExecutionContext.execute_operation()

buildするタイミングでdefaultではExecutionContextのインスタンス上に良い感じにアクセス関数が渡される。

```
        return cls(
            schema,
            fragments,
            root_value,
            context_value,
            operation,
            coerced_variable_values,  # coerced values
            field_resolver or default_field_resolver,  # この辺
            type_resolver or default_type_resolver,　＃この辺
            [],
            middleware_manager,
        )

```



```python
    def execute_operation(
        self, operation: OperationDefinitionNode, root_value: Any
    ) -> Optional[AwaitableOrValue[Any]]:
        """Execute an operation.

        Implements the "Evaluating operations" section of the spec.
        """
        type_ = get_operation_root_type(self.schema, operation)
        fields = self.collect_fields(type_, operation.selection_set, {}, set())

        path = None

        # Errors from sub-fields of a NonNull type may propagate to the top level, at
        # which point we still log the error and null the parent field, which in this
        # case is the entire response.
        #
        # Similar to complete_value_catching_error.
        try:
            result = (
                self.execute_fields_serially
                if operation.operation == OperationType.MUTATION
                else self.execute_fields
            )(type_, root_value, path, fields)
        except GraphQLError as error:
            self.errors.append(error)
            return None
        else:
            if isawaitable(result):
                # noinspection PyShadowingNames
                async def await_result():
                    try:
                        return await result  # type: ignore
                    except GraphQLError as error:
                        self.errors.append(error)

                return await_result()
            return result
 ```

queryの場合には `execute_fields()`

```
    def execute_fields(
        self,
        parent_type: GraphQLObjectType,
        source_value: Any,
        path: Optional[Path],
        fields: Dict[str, List[FieldNode]],
    ) -> AwaitableOrValue[Dict[str, Any]]:
        """Execute the given fields concurrently.

        Implements the "Evaluating selection sets" section of the spec for "read" mode.
        """
        results = {}
        awaitable_fields: List[str] = []
        append_awaitable = awaitable_fields.append
        for response_name, field_nodes in fields.items():
            field_path = Path(path, response_name)
            result = self.resolve_field(
                parent_type, source_value, field_nodes, field_path
            )
            if result is not Undefined:
                results[response_name] = result
                if isawaitable(result):
                    append_awaitable(response_name)

        #  If there are no coroutines, we can just return the object
        if not awaitable_fields:
            return results

        # Otherwise, results is a map from field name to the result of resolving that
        # field, which is possibly a coroutine object. Return a coroutine object that
        # will yield this same map, but with any coroutines awaited in parallel and
        # replaced with the values they yielded.
        async def get_results():
            results.update(
                zip(
                    awaitable_fields,
                    await gather(*(results[field] for field in awaitable_fields)),
                )
            )
            return results

        return get_results()
```

resolve field

```python
    def resolve_field(
        self,
        parent_type: GraphQLObjectType,
        source: Any,
        field_nodes: List[FieldNode],
        path: Path,
    ) -> AwaitableOrValue[Any]:
        """Resolve the field on the given source object.

        In particular, this figures out the value that the field returns by calling its
        resolve function, then calls complete_value to await coroutine objects,
        serialize scalars, or execute the sub-selection-set for objects.
        """
        field_node = field_nodes[0]
        field_name = field_node.name.value

        field_def = get_field_def(self.schema, parent_type, field_name)
        if not field_def:
            return Undefined

        resolve_fn = field_def.resolve or self.field_resolver

        if self.middleware_manager:
            resolve_fn = self.middleware_manager.get_field_resolver(resolve_fn)

        info = self.build_resolve_info(field_def, field_nodes, parent_type, path)

        # Get the resolve function, regardless of if its result is normal or abrupt
        # (error).
        result = self.resolve_field_value_or_error(
            field_def, field_nodes, resolve_fn, source, info
        )

        return self.complete_value_catching_error(
            field_def.type, field_nodes, info, path, result
        )
```

それぞれのfieldに対して、resolverが定義されてなかったら、Contextが持つfield_resolverが呼ばれる。

これはこういう定義（だった） https://github.com/graphql-python/graphql-core/blob/fc5ad0bd527368f395471c685716643f6957caec/src/graphql/execution/execute.py#L1147

```python
def default_field_resolver(source, info, **args):
    """Default field resolver.

    If a resolve function is not given, then a default resolve behavior is used which
    takes the property of the source object of the same name as the field and returns
    it as the result, or if it's a function, returns the result of calling that function
    while passing along args and context.

    For dictionaries, the field names are used as keys, for all other objects they are
    used as attribute names.
    """
    # Ensure source is a value for which property access is acceptable.
    field_name = info.field_name
    value = (
        source.get(field_name)
        if isinstance(source, dict)
        else getattr(source, field_name, None)
    )
    if callable(value):
        return value(info, **args)
    return value
```
