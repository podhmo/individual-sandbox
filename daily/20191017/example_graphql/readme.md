# graphql

そもそもschema以外の内容を把握しきれていない？([00schema.graphql](00schema.graphql))

- schema, type Query, type Todo
- schemaをexports的なものとして見ると良いのかもしれない。

## graphql-core-next

graphql-core-nextはgraphql-core>3

```console
$ python -m pip install "graphql-core>=3a"
```

とりあえずかんたんなsync,asyncのhelloは書いた。
個別に対応する型のオブジェクトを使うのは地味に面倒では？

- 引数を取るのが面倒

## graphene

何かORMっぽく書けるっぽいやつっぽい。
あんまり必要性がわからない。

## asgi test client

ariadneのテストで欲しくなった。この辺使えるならそれで。

- https://github.com/vinissimus/async-asgi-testclient

できた。でもこの警告なんで出るんだろう？

```
04ariadne/test_main.py::test_app PASSED                                                           [100%]

=========================================== warnings summary ============================================
VENV/lib/python3.7/site-packages/async_asgi_testclient/testing.py:51
  VENV/lib/python3.7/site-packages/async_asgi_testclient/testing.py:51: PytestCollectionWarning: cannot collect test class 'Client' because it has a __init__ constructor (from: 04ariadne/test_main.py)
    class Client:

-- Docs: https://docs.pytest.org/en/latest/warnings.html
===================================== 1 passed, 1 warnings in 0.13s =====================================
```

このへんで出るっぽい `pytest._pytest.python`

```python
@hookimpl(hookwrapper=True)
def pytest_pycollect_makeitem(collector, name, obj):

# ...

        if not (inspect.isfunction(obj) or inspect.isfunction(get_real_func(obj))):
            filename, lineno = getfslineno(obj)
            warnings.warn_explicit(
                message=PytestCollectionWarning(
                    "cannot collect %r because it is not a function." % name
                ),
                category=None,
                filename=str(filename),
                lineno=lineno + 1,
            )
```

## graphql-relay?

