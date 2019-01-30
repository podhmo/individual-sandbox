import json
from collections import namedtuple
cmd = namedtuple("cmd", "op, v, v1")

# todo: move


def unpatch(src, dst, *, verbose=False):
    r = Walker().walk(src, dst)
    rows = merge(r)

    if not verbose:
        for row in rows:
            row.pop("from", None)
            if row["op"] == "remove":
                row.pop("value", None)
            yield row
    else:
        yield from rows


def merge(r):
    if r is None:
        return []
    if not hasattr(r, "keys"):
        yield {"path": "/", "op": r.op, "value": r.v, "from": r.v1}
    else:
        for k, v in r.items():
            for sv in merge(v):
                yield {
                    "path": f"/{k}/{sv['path'].lstrip('/')}".rstrip("/"),  # xxx
                    "op": sv["op"],
                    "value": sv["value"],
                    "from": sv.pop("from", None),
                }


class Walker:
    # two path scan move, copy
    def __init__(self):
        self.move_map = {}  # todo:

    def walk(self, src, dst):
        if isinstance(src, (list, tuple)):
            src = dict(enumerate(src))
        if isinstance(dst, (list, tuple)):
            dst = dict(enumerate(dst))

        # xxx: src and dst is None
        if hasattr(src, "keys"):
            return self._walk_dict(src, dst)
        else:
            return self._walk_atom(src, dst)

    def _walk_dict(self, src, dst):
        r = {}
        for k, v in src.items():
            if k in dst:
                r[k] = self.walk(v, dst[k])
            else:
                r[k] = cmd("remove", v=v, v1=None)
        for k, v in dst.items():
            if k in r:
                continue
            r[k] = cmd("add", v=v, v1=None)
        return r

    def _walk_atom(self, src, dst):
        if src is None:
            return cmd(op="add", v=dst, v1=None)
        elif dst is None:
            return cmd(op="remove", v=src, v1=None)
        elif src != dst:
            return cmd(op="replace", v=dst, v1=src)
        else:
            return None


import unittest  # noqa


class Tests(unittest.TestCase):
    def _callFUT(self, src, dst):
        return unpatch(src, dst)

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
                    "path": "/",
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
