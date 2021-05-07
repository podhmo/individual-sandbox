import json


class E(Exception):
    pass


try:
    json.loads("{")
except json.JSONDecodeError as e:
    raise E(f"{e.__class__.__name__}: {e}")
