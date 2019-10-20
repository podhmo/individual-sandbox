```console
$ pytest
===================================== test session starts =====================================
platform linux -- Python 3.7.4, pytest-5.2.1, py-1.7.0, pluggy-0.12.0
rootdir: VENV/daily/20191020/example_asyncio
plugins: cov-2.6.1, asyncio-0.10.0
collected 1 item                                                                              

test_asgi.py .                                                                          [100%]

====================================== warnings summary =======================================
VENV/lib/python3.7/site-packages/async_asgi_testclient/testing.py:51
  VENV/lib/python3.7/site-packages/async_asgi_testclient/testing.py:51: PytestCollectionWarning: cannot collect test class 'TestClient' because it has a __init__ constructor (from: test_asgi.py)
    class TestClient:

-- Docs: https://docs.pytest.org/en/latest/warnings.html
================================ 1 passed, 1 warnings in 0.06s ================================
```
