import yaml
import sys
import io

code = """\
properties:
  name:
    type: string
  status:
    type: string
    enum:
      - ok # success
      - ng # failure
"""


d = yaml.load(io.StringIO(code), Loader=yaml.SafeLoader)
print(d)
yaml.dump(d, stream=sys.stdout, Dumper=yaml.SafeDumper)
