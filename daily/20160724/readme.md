# `python setup.py test` で djangoのライブラリをテスト出来るようにする方法

django-returnfieldsを作った時などに使ったもの。忘れても良い事なのでここにメモして忘れる。
django-fooというパッケージがその例。

sturucture

```
$ tree
.
├── django_foo
│   ├── __init__.py
│   └── tests
│       ├── __init__.py
│       ├── models.py
│       ├── settings.py
│       └── test_it.py
└── setup.py
```

running test

```
$ python setup.py test
....
----------------------------------------------------------------------
Ran 4 tests in 0.026s

OK
Creating test database for alias 'default'...
Destroying test database for alias 'default'...
```

running test, only target test_method.

```
$ python setup.py test -s django_foo.tests.test_it.ModelTests.test_query_with_prefetch
.
----------------------------------------------------------------------
Ran 1 test in 0.017s

OK
Creating test database for alias 'default'...
Destroying test database for alias 'default'...
```

