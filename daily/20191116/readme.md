## metashape

もう少し引数や属性を用意しても良いのかもしれない。

### hmm

- openapiのContextのstatusの名前は変えておきたいかも
- Contextの属性がわからないのが不便かも

scanした結果を使って、何かをしたいのだけど。
このときどうするかどうかという話だよなー。

あー、result.storeという名前がおかしい？

### 追記

- 使用感としてcontextの属性は微妙
- xxx

## python fastapi DIの仕組みを覗いてみる？

fastapi(古くはapistar)のDIの仕組みを覗いてみると良いかもしれない

たぶんこの辺りがDI (solve_dependencies())

https://github.com/tiangolo/fastapi/blob/master/fastapi/dependencies/utils.py#L372

routing.pyで使われているみたいだけれど。実際に関数に引数を注入している部分がわからない。

### 追記

もう少しまじめに中を覗く。
`fastapi.applicatons:FastAPI::get()` で何が行われているかを把握すると良いかもしれない。
これは`fastapi.routing:APIRouter.get()`を読んでいる。
そしてこれは `fastapi.routing:APIRouter.api_router()` を呼び、これは `fastapi.routing:APIRouter.add_api_router()` を呼ぶdecoratorで包んで返している。
結局これはRouteクラスを作ってそれを`self.routes.append(route)` している。

### 追記

APIRouterの `self.routes` とはなんだろう？

```python
from starlette import routing

class APIRouter(routing.Router):
    def __init__(
        self,
        routes: List[routing.BaseRoute] = None,
...
        route_class: Type[APIRoute] = APIRoute,
    ) -> None:
        super().__init__(
            routes=routes, redirect_slashes=redirect_slashes, default=default
        )
        self.dependency_overrides_provider = dependency_overrides_provider
        self.route_class = route_class
```

つまり `starlette.routing:Router` のサブクラス。DIの解釈はおそらくRouteクラスで行われていそう。このあたりは関係なさそう。`fastapi.routing:APIRoute`。

### 追記

あー、本体は`fastapi.dependecies.models:Dependant`なのか。これはただのオブジェクト。`fastapi.dependecies.utils:get_typed_signature()`でsignatureを取り出している。

情報を良い感じにマーキングするのは `fastapi.dependencies.utils:get_dependant()` っぽい。内部的には `Depends` のサブクラスなら依存として扱う。それらはdependeciesとして集められる。

ただ良い感じのオブジェクトとしてまとめられるだけだな。コレを解決する部分は。`fastapi.routing:get_request_handler()`ここで `fastapi.dependencie.utils:solve_depencies()`これで解決された感じの結果が返す感じっぽい。

そして事前に解決するのではなく解決するappでwrapするのか。

### 追記

概ね理解した

## hmm

へー。

```python
def get_typed_annotation(param: inspect.Parameter, globalns: Dict[str, Any]) -> Any:
    annotation = param.annotation
    if isinstance(annotation, str):
        annotation = ForwardRef(annotation)
        annotation = evaluate_forwardref(annotation, globalns, globalns)
    return annotation
```

