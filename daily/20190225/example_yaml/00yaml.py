import sys
import yaml

d = {"data": {"package": "hello-0.0.0#master"}}
yaml.dump(d, sys.stdout)
# data: {package: hello-0.0.0#master}

yaml.dump(d, sys.stdout, default_flow_style=False, allow_unicode=True)
# data:
#   package: hello-0.0.0#master

from io import StringIO

o = StringIO()
yaml.dump(d, sys.stdout, default_flow_style=False, allow_unicode=True)
o.seek(0)
print(o.getvalue())
o.seek(0)
print(yaml.load(o))
