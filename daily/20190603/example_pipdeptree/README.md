```console
$ make -n
rm -rf venv/foo
rm -f *.lock *.deps
mkdir -p venv/foo
python -m venv venv/foo
./venv/foo/bin/pip install -r requirements.txt
make all-requirements pyramid-requirements flask-requirements show-deps
venv/foo/bin/pip freeze | sed 's/^ *//g' | sort -u | tee requirements.lock
venv/foo/bin/pipdeptree -p pyramid,mako,sqlalchemy -f | sed 's/^ *//g' | sort -u | tee pyramid-requirements.lock
venv/foo/bin/pipdeptree -p flask,mako,sqlalchemy -f | sed 's/^ *//g' | sort -u | tee flask-requirements.lock
venv/foo/bin/pipdeptree | tee all.deps
make diff
diff -u requirements.lock pyramid-requirements.lock > pyramid.diff || echo ok
diff -u requirements.lock flask-requirements.lock > flask.diff || echo ok
```
