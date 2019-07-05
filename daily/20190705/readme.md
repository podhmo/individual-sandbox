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

## python timing-asgiってなに？

https://github.com/steinnes/timing-asgi
