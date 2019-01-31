import json
from collections import namedtuple
diff = namedtuple("diff", "op, value, x_from, x_to")

# todo: move
sentinel = object()

def unpatch(src, dst, *, verbose=False):
    r = Walker().walk(src, dst)
    rows = merge(r)

    if not verbose:
        for row in rows:
            if row["op"] == "remove":
                row.pop("value", None)
            for k in list(row.keys()):
                if k.startswith("x_"):
                    row.pop(k)
            yield row
    else:
        for row in rows:
            if row["op"] == "remove":
                row.pop("value", None)
                row.pop("x_to", None)
            elif row["op"] == "add":
                row.pop("x_from", None)
                row.pop("x_to", None)
            elif row["op"] == "replace":
                row.pop("x_to", None)
            for k in list(row.keys()):
                if not k.startswith("x_"):
                    continue
                row[k.replace("x_", "x-")] = row.pop(k)
            yield row


def merge(r):
    if r is None:
        return []
    if not hasattr(r, "keys"):
        yield {"path": "", **r._asdict()}
    else:
        for k, v in r.items():
            prefix = str(k).replace("~", "~0").replace("/", "~1")
            for sv in merge(v):
                sv["path"] = f"/{prefix}/{sv['path'].lstrip('/')}".rstrip("/")
                yield sv


class Walker:
    # two path scan move, copy
    def __init__(self):
        self.move_map = {}  # todo:

    def walk(self, src, dst):
        # xxx: src and dst is None
        if hasattr(src, "keys"):
            return self._walk_dict(src, dst)
        elif isinstance(src, (list, tuple)):
            return self._walk_list(src, dst)
        else:
            return self._walk_atom(src, dst)

    def _walk_list(self, src, dst):
        r = {}
        try:
            n = min(len(src), len(dst))
        except TypeError:
            return diff(op="replace", value=dst, x_from=src, x_to=dst)
        for i in range(n):
            r[str(i)] = self.walk(src[i], dst[i])

        if n == len(dst):
            for i in range(n, len(src)):
                r[str(i)] = diff(op="remove", value=None, x_from=src[i], x_to=None)
        else:
            for i in range(n, len(dst)):
                r[str(i)] = diff(op="add", value=dst[i], x_from=None, x_to=dst[i])
        return r

    def _walk_dict(self, src, dst):
        r = {}
        for k, v in src.items():
            if k in dst:
                r[k] = self.walk(v, dst[k])
            else:
                r[k] = diff("remove", value=None, x_from=v, x_to=None)
        for k, v in dst.items():
            if k in r:
                continue
            r[k] = diff("add", value=v, x_from=None, x_to=v)
        return r

    def _walk_atom(self, src, dst):
        if src is None:
            return diff(op="add", value=dst, x_from=None, x_to=dst)
        elif dst is None:
            return diff(op="remove", value=None, x_from=src, x_to=None)
        elif src != dst:
            return diff(op="replace", value=dst, x_from=src, x_to=dst)
        else:
            return None


import unittest  # noqa


class Tests(unittest.TestCase):
    def _callFUT(self, src, dst):
        return unpatch(src, dst)

    maxDiff = None

    def test(self):
        import jsonpatch

        C = namedtuple("C", "src, dst, want, skip_patch")
        cases = [
            C(
                src={"name": "foo"},
                dst={"name": "foo"},
                want=[],
                skip_patch=False,
            ),
            C(
                src={"name": "foo"},
                dst={"name": "bar"},
                want=[{
                    "op": "replace",
                    "path": "/name",
                    "value": "bar"
                }],
                skip_patch=False,
            ),
            C(
                src={"name": "foo"},
                dst={},
                want=[{
                    "op": "remove",
                    "path": "/name"
                }],
                skip_patch=False,
            ),
            C(
                src={},
                dst={"name": "bar"},
                want=[{
                    "op": "add",
                    "path": "/name",
                    "value": "bar"
                }],
                skip_patch=False,
            ),
            C(
                src={"point0": {
                    "value": 10
                }},
                dst={"point1": {
                    "value": 10
                }},
                # todo: move
                want=[
                    {
                        "op": "remove",
                        "path": "/point0",
                    },
                    {
                        "op": "add",
                        "path": "/point1",
                        "value": {
                            "value": 10
                        },
                    },
                ],
                skip_patch=False,
            ),
            C(
                src={"point0": {
                    "value": 10
                }},
                dst={"point0": {
                    "value": 20
                }},
                want=[
                    {
                        "op": "replace",
                        "path": "/point0/value",
                        "value": 20,
                    },
                ],
                skip_patch=False,
            ),
            C(
                src={"person": {
                    "name": "foo",
                    "age": 20,
                    "type": "P"
                }},
                dst={"person": {
                    "name": "bar",
                    "nickname": "B",
                    "type": "P"
                }},
                want=[
                    {
                        "op": "replace",
                        "path": "/person/name",
                        "value": "bar",
                    },
                    {
                        "op": "remove",
                        "path": "/person/age",
                    },
                    {
                        "op": "add",
                        "path": "/person/nickname",
                        "value": "B",
                    },
                ],
                skip_patch=False,
            ),
            C(
                src=[{}, {
                    "person": {
                        "name": "foo",
                        "age": 20,
                        "type": "P"
                    }
                }],
                dst=[{
                    "person": {
                        "name": "bar",
                        "nickname": "B",
                        "type": "P"
                    }
                }, {}],
                want=[
                    {
                        "path": "/0/person",
                        "op": "add",
                        "value": {
                            "name": "bar",
                            "nickname": "B",
                            "type": "P"
                        }
                    },
                    {
                        "path": "/1/person",
                        "op": "remove",
                    },
                ],
                skip_patch=False,
            ),
            C(
                src=None,
                dst=1,
                want=[{
                    "op": "add",
                    "path": "",  # root is ""
                    "value": 1
                }],
                skip_patch=True,
            ),
            ## runtime error
            # C(
            #     src={},
            #     dst=1,
            #     want=[{
            #         "op": "add",
            #         "path": "/",
            #         "value": 1
            #     }],
            #     skip_patch=True,
            # )
            C(
                src={"v": 1},
                dst={"v": [1]},
                want=[{
                    "op": "replace",
                    "path": "/v",
                    "value": [1],
                }],
                skip_patch=False,
            ),
            C(
                src={"v": [1]},
                dst={"v": 1},
                want=[{
                    "op": "replace",
                    "path": "/v",
                    "value": 1,
                }],
                skip_patch=False,
            ),
            C(
                src={
                    "name": "foo",
                    "age": 20
                },
                dst={"person": {
                    "name": "foo",
                    "age": 20
                }},
                # move ?
                want=[
                    {
                        "op": "add",
                        "path": "/person",
                        "value": {
                            "age": 20,
                            "name": "foo"
                        }
                    },
                    {
                        "op": "remove",
                        "path": "/age"
                    },
                    {
                        "op": "remove",
                        "path": "/name"
                    },
                ],
                skip_patch=False,
            ),
            C(
                src={
                    "person": {
                        "name": "foo",
                        "age": 20,
                    },
                },
                dst={
                    "name": "foo",
                    "age": 20
                },
                # move ?
                want=[
                    {
                        "op": "remove",
                        "path": "/person",
                    },
                    {
                        "op": "add",
                        "path": "/age",
                        "value": 20,
                    },
                    {
                        "op": "add",
                        "path": "/name",
                        "value": "foo"
                    },
                ],
                skip_patch=False,
            )
        ]

        for i, c in enumerate(cases):
            with self.subTest(i):
                got = list(self._callFUT(c.src, c.dst))
                if not c.skip_patch:
                    self.assertEqual(jsonpatch.JsonPatch(got).apply(c.src), c.dst)
                self.assertListEqual(
                    sorted([json.dumps(x, sort_keys=True) for x in got]),
                    sorted([json.dumps(x, sort_keys=True) for x in c.want])
                )


if __name__ == "__main__":
    unittest.main()
