## python graphql ariadne 動かしてみる

- https://ariadnegraphql.org/
- https://ariadnegraphql.org/docs/intro.html
- https://ariadnegraphql.org/docs/starlette-integration.html

```console
$ pip install ariadne uvicorn
```

### とりあえずどうしようかな

- introductionを動かす(uvicorn)
- starlette/fastapi越しに動かしてみる
- graphql-core-nextを直接使ってみる

ariadneがasgi appを作ってくれるので、uvicornとariadne前提にしたコードはけっこう手軽に作れそう。

### introduction

基本的には `make_executable_schema` でschemaを作ってここからasgi appを作る形。

```python
schema = make_executable_schema(type_defs, query)
app = GraphQL(schema, debug=True)
```

uvicornでてきとうに実行する(gunicorn的な記法で)

```console
$ uvicorn <filename without .py>:<asgi app name>
# e.g. uvicorn myscript:app
```

type_defsはschema定義

```python
type_defs = gql(
    """
type Query {
  hello: String!
}
"""
)
```

ariadne.gqlはvalidation。

queryの方はinstance作ってresolverを登録するもの。

```python
query = QueryType()

@query.field("hello")
def resolve_hello(_, info) -> str:
    request = info.context["request"]
    user_agent = request.headers.get("user-agent", "guest")
    return f"Hello, {user_agent}!"
```

root resolverのときには第一引数に意味がない(`_`にしている)。

### graphql-core-nextで動かす

どうもgraphql-core-nextは評価器的なもののよう？

https://github.com/graphql-python/graphql-core-next

たとえば↑の例で載っているのは、以下のようなawaitable function

```python
async def graphql(
    schema: GraphQLSchema,
    source: Union[str, Source],
) -> ExecutionResult:
```

そして、以下の様な形で作ったschemaを渡すイメージ。

```python
schema = GraphQLSchema(
    query=GraphQLObjectType(
        name="RootQueryType",
        fields={"hello": GraphQLField(GraphQLString, resolve=resolve_hello)},
    )
)
```

実行するのは以下の様な形。

```python
async def main():
    query = "{ hello }"
    result = await graphql(schema, query)
    print(result) ## ExecutionResult
```
