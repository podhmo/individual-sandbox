import sys
import io
import ruamel.yaml
from dictknife import DictWalker


yaml = ruamel.yaml.YAML()  # defaults to round-trip

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


d = yaml.load(io.StringIO(code))
for path, sd in DictWalker(["type"]).walk(d):
    if sd["type"] == "string":
        sd["format"] = "text"
print(d)
yaml.dump(d, stream=sys.stdout)
