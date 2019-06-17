## python fastapi

`/openapi.json` のendpointってどうやって実装されているんだろう？

- server立ち上げずに取り出せない？
- x-pydantic-validatorとか指定したい

### serverを立ち上げずopenapi.jsonを取り出す

fastapi/applications.py

```python
class FastAPI(Starlette):
...
    def openapi(self) -> Dict:
        if not self.openapi_schema:
            self.openapi_schema = get_openapi(
                title=self.title,
                version=self.version,
                openapi_version=self.openapi_version,
                description=self.description,
                routes=self.routes,
                openapi_prefix=self.openapi_prefix,
            )
        return self.openapi_schema
```

`get_openapi()` は 以下。

```python
from fastapi.openapi.utils import get_openapi
```

なのでとりあえずopenapi documentが欲しい場合には以下で済む。

```python
print(app.openapi())
```

### openapi.jsonをイジる

けっこうopenapi/utils.pyに色々ガッと定義されている。

```python
def get_openapi(
...
    for route in routes:
        if isinstance(route, routing.APIRoute):
            result = get_openapi_path(route=route, model_name_map=model_name_map)


def get_openapi_path(
    *, route: routing.APIRoute, model_name_map: Dict[Type, str]
) -> Tuple[Dict, Dict, Dict]:
    if route.include_in_schema:
        for method in route.methods:
...
            all_route_params = get_openapi_params(route.dependant)
            validation_definitions, operation_parameters = get_openapi_operation_parameters(
                all_route_params=all_route_params
            )
            definitions.update(validation_definitions)
            parameters.extend(operation_parameters)
```

作りたいのはschemas/userの部分っぽいな。

### model部分のschemas

以下なので openapi/utils.py ではなく fastapi/utils.pyも見る必要がある。

```python
from fastapi.utils import get_flat_models_from_routes, get_model_definitions
```

openapi/utils.py

```python
from pydantic.schema import get_flat_models_from_fields, model_process_schema

def get_model_definitions(
    *, flat_models: Set[Type[BaseModel]], model_name_map: Dict[Type[BaseModel], str]
) -> Dict[str, Any]:
    definitions: Dict[str, Dict] = {}
    for model in flat_models:
        m_schema, m_definitions = model_process_schema(
            model, model_name_map=model_name_map, ref_prefix=REF_PREFIX
        )
        definitions.update(m_definitions)
        model_name = model_name_map[model]
        definitions[model_name] = m_schema
    return definitions
```

ふつうにmodel_process_schema? pydanticの話かも。


