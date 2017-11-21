```
$ VERSION=2.9.6 make checktime
(pip install --force-reinstall Jinja2==2.9.6 || exit 0)
Requirement already satisfied: Jinja2==2.9.6 in ~/venv/web/lib/python3.6/site-packages
Requirement already satisfied: MarkupSafe>=0.23 in ~/venv/web/lib/python3.6/site-packages (from Jinja2==2.9.6)
time python -c 'import jinja2'
        0.45 real         0.33 user         0.04 sys
time python -c 'import jinja2'
        0.44 real         0.34 user         0.04 sys
time python -c 'import jinja2'
        0.47 real         0.35 user         0.04 sys
$ make checktime
(pip install --force-reinstall Jinja2==2.10 || exit 0)
Collecting Jinja2==2.10
  Using cached Jinja2-2.10-py2.py3-none-any.whl
Requirement already satisfied: MarkupSafe>=0.23 in ~/venv/web/lib/python3.6/site-packages (from Jinja2==2.10)
Installing collected packages: Jinja2
  Found existing installation: Jinja2 2.9.6
    Uninstalling Jinja2-2.9.6:
      Successfully uninstalled Jinja2-2.9.6
Successfully installed Jinja2-2.10
time python -c 'import jinja2'
        0.21 real         0.12 user         0.02 sys
time python -c 'import jinja2'
        0.25 real         0.14 user         0.02 sys
time python -c 'import jinja2'
        0.25 real         0.16 user         0.03 sys
```
