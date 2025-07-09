# report

```
┏━━━━━━━━━━━━┳━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━┳━━━━━━━━━━━━━┓
┃ Name       ┃ Type    ┃ Envs              ┃ Dependencies ┃ Scripts     ┃
┡━━━━━━━━━━━━╇━━━━━━━━━╇━━━━━━━━━━━━━━━━━━━╇━━━━━━━━━━━━━━╇━━━━━━━━━━━━━┩
│ hatch-test │ virtual │ hatch-test.py3.10 │ black        │ cov-combine │
│            │         │ hatch-test.py3.11 │ pytest       │ cov-report  │
│            │         │ hatch-test.py3.12 │              │ run         │
│            │         │ hatch-test.py3.13 │              │ run-cov     │
└────────────┴─────────┴───────────────────┴──────────────┴─────────────┘
```

## test

```
─────────────────────────────────────────────────── hatch-test.py3.10 ────────────────────────────────────────────────────
============================= test session starts ==============================
platform darwin -- Python 3.10.16, pytest-8.4.1, pluggy-1.6.0
rootdir: /private/tmp/foo
configfile: pyproject.toml
** lib2to3 **
collected 1 item

test_it.py .

=============================== warnings summary ===============================
test_it.py:2
  /private/tmp/foo/test_it.py:2: PendingDeprecationWarning: lib2to3 package is deprecated and may not be able to parse Python 3.10+
    import lib2to3

-- Docs: https://docs.pytest.org/en/stable/how-to/capture-warnings.html
========================= 1 passed, 1 warning in 0.00s =========================
─────────────────────────────────────────────────── hatch-test.py3.11 ────────────────────────────────────────────────────
============================= test session starts ==============================
platform darwin -- Python 3.11.13, pytest-8.4.1, pluggy-1.6.0
rootdir: /private/tmp/foo
configfile: pyproject.toml
** lib2to3 **
collected 1 item

test_it.py .

=============================== warnings summary ===============================
test_it.py:2
  /private/tmp/foo/test_it.py:2: DeprecationWarning: lib2to3 package is deprecated and may not be able to parse Python 3.10+
    import lib2to3

-- Docs: https://docs.pytest.org/en/stable/how-to/capture-warnings.html
========================= 1 passed, 1 warning in 0.00s =========================
─────────────────────────────────────────────────── hatch-test.py3.12 ────────────────────────────────────────────────────
============================= test session starts ==============================
platform darwin -- Python 3.12.8, pytest-8.4.1, pluggy-1.6.0
rootdir: /private/tmp/foo
configfile: pyproject.toml
** lib2to3 **
collected 1 item

test_it.py .

=============================== warnings summary ===============================
test_it.py:2
  /private/tmp/foo/test_it.py:2: DeprecationWarning: lib2to3 package is deprecated and may not be able to parse Python 3.10+
    import lib2to3

-- Docs: https://docs.pytest.org/en/stable/how-to/capture-warnings.html
========================= 1 passed, 1 warning in 0.00s =========================
─────────────────────────────────────────────────── hatch-test.py3.13 ────────────────────────────────────────────────────
============================= test session starts ==============================
platform darwin -- Python 3.13.4, pytest-8.4.1, pluggy-1.6.0
rootdir: /private/tmp/foo
configfile: pyproject.toml
** blib2to3 **
collected 1 item

test_it.py .

============================== 1 passed in 0.00s ===============================
```
