## python fastapi

`/openapi.json` のendpointってどうやって実装されているんだろう？

- server立ち上げずに取り出せない？
- x-pydantic-validatorとか指定したい

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
