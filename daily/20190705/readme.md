## fast api

https://github.com/tiangolo/fastapi/issues/360

これをちょっと見てみるか。

## python pyproject.toml with `pip install -e`

pyproject.tomlだけのパッケージって `pip install -e`できなかったんだ。

```
$ pip install -e .
File "setup.py" not found. Directory cannot be installed in editable mode: $HOME/venvs/fastapi/fastapi
(A "pyproject.toml" file was found, but editable mode currently requires a setup.py based build.)
```

直接installするには以下の様にすれば良いみたいだけれど。

```console
$ pip install git+https://github.com/euri10/fastapi.git@slow_serial
```

依存を入れて、以下が動けば良い。

```console
$ pip install pytest starlette pydantic faker requests timing_asgi
$ pytest tests/test_serial_speed.py --log-cli-level=INFO
```

たしかにその程度の時間がかかる。

```
============================= test session starts ==============================
platform linux -- Python 3.7.3, pytest-5.0.0, py-1.8.0, pluggy-0.12.0
rootdir: /home/nao/venvs/fastapi/fastapi

----------------------------- live log collection ------------------------------
WARNING  fastapi:models.py:17 email-validator not installed, email fields will be treated as str.
                              To install, run: pip install email-validator
collected 1 item

tests/test_serial_speed.py::test_routes 
-------------------------------- live log call ---------------------------------
INFO     tests.test_serial_speed:test_serial_speed.py:41 route1: 0.03104877471923828
INFO     tests.test_serial_speed:test_serial_speed.py:19 app.tests.test_serial_speed.route1, 2.925880193710327, ['http_status:200', 'http_method:GET', 'time:wall']
INFO     tests.test_serial_speed:test_serial_speed.py:19 app.tests.test_serial_speed.route1, 2.9227090000000002, ['http_status:200', 'http_method:GET', 'time:cpu']
INFO     tests.test_serial_speed:test_serial_speed.py:54 route1: 0.03025960922241211
INFO     tests.test_serial_speed:test_serial_speed.py:19 app.tests.test_serial_speed.route2, 3.260159730911255, ['http_status:200', 'http_method:GET', 'time:wall']
INFO     tests.test_serial_speed:test_serial_speed.py:19 app.tests.test_serial_speed.route2, 3.2563330000000006, ['http_status:200', 'http_method:GET', 'time:cpu']
INFO     tests.test_serial_speed:test_serial_speed.py:66 route1: 0.031212329864501953
INFO     tests.test_serial_speed:test_serial_speed.py:19 app.tests.test_serial_speed.route3, 3.5656020641326904, ['http_status:200', 'http_method:GET', 'time:wall']
INFO     tests.test_serial_speed:test_serial_speed.py:19 app.tests.test_serial_speed.route3, 3.5615059999999996, ['http_status:200', 'http_method:GET', 'time:cpu']
PASSED                                                                   [100%]

=========================== 1 passed in 9.99 seconds ===========================
```

### 中をまじめに覗いてみる

diff routing1 -> routing2

```diff
--- test_route01.py	2019-07-05 16:50:33.875049986 +0900
+++ test_route02.py	2019-07-05 16:51:24.298501750 +0900
@@ -1,5 +1,7 @@
+from typing import List, Any
 import time
 from starlette.testclient import TestClient
+from pydantic import BaseModel
 from faker import Faker
 from app import app
 
@@ -9,7 +11,11 @@
 client = TestClient(app)
 
 
-@app.get("/route")
+class BigData(BaseModel):
+    key: List[List[Any]]
+
+
+@app.get("/route", response_model=BigData)
 def route():
     bigdata = {
         "key": [
```

diff routing1 -> routing3

```diff
--- test_route01.py	2019-07-05 16:50:33.875049986 +0900
+++ test_route03.py	2019-07-05 16:52:08.605272468 +0900
@@ -1,5 +1,7 @@
+from typing import List, Any
 import time
 from starlette.testclient import TestClient
+from pydantic import BaseModel
 from faker import Faker
 from app import app
 
@@ -9,8 +11,12 @@
 client = TestClient(app)
 
 
-@app.get("/route")
-def route():
+class BigData(BaseModel):
+    key: List[List[Any]]
+
+
+@app.get("/route", response_model=BigData)
+def route3():
     bigdata = {
         "key": [
             [fake.email for i in range(X)],
@@ -19,7 +25,7 @@
             [fake.password for i in range(X)],
         ]
     }
-    return bigdata
+    return BigData(**bigdata)
 
 
 def test_routes():
```

そもそもこれはpydanticのはなしじゃない？

### response_modelの実装

routing.py

```python
class APIRoute(routing.Route):
# ...
        if self.response_model:
            assert lenient_issubclass(
                response_class, JSONResponse
            ), "To declare a type the response must be a JSON response"
            response_name = "Response_" + self.name
            self.response_field: Optional[Field] = Field(
                name=response_name,
                type_=self.response_model,
                class_validators={},
                default=None,
                required=False,
                model_config=BaseConfig,
                schema=Schema(None),
            )
        else:
            self.response_field = None
```

実際にはココらへんで使われている。

```python
def serialize_response(
    *,
    field: Field = None,
    response: Response,
    include: Set[str] = None,
    exclude: Set[str] = set(),
    by_alias: bool = True,
    skip_defaults: bool = False,
) -> Any:
    if field:
        errors = []
        value, errors_ = field.validate(response, {}, loc=("response",))
        if isinstance(errors_, ErrorWrapper):
            errors.append(errors_)
        elif isinstance(errors_, list):
            errors.extend(errors_)
        if errors:
            raise ValidationError(errors)
        return jsonable_encoder(
            value,
            include=include,
            exclude=exclude,
            by_alias=by_alias,
            skip_defaults=skip_defaults,
        )
    else:
        return jsonable_encoder(response)
```

## python timing-asgiってなに？

https://github.com/steinnes/timing-asgi
