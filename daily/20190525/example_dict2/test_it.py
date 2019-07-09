import unittest
import copy
import json
import itertools
from collections import namedtuple
import os

COLSIZE = int(os.environ.get("COLSIZE") or "60")


class Tests(unittest.TestCase):
    def _callFUT(self, *args, **kwargs):
        from merge import merge

        return list(merge(*args, **kwargs))

    def test_it(self):
        from merge import how_left_outer_join, how_right_outer_join, how_full_outer_join

        # no name change (e.g. users.name, skills.name)
        # (id is conflicted, on merge, skipped)
        class data:
            groups = [
                {"id": 1, "gname": "A"},
                {"id": 2, "gname": "B"},
                {"id": 3, "gname": "C"},
            ]
            users = [
                {"id": 10, "name": "Ax", "gid": 1},
                {"id": 11, "name": "Ay", "gid": 1},
                {"id": 20, "name": "Bi", "gid": 2},
                {"id": 40, "name": "D?", "gid": 4},
            ]

        class copied:
            groups = copy.deepcopy(data.groups)
            users = copy.deepcopy(data.users)

        C = namedtuple("C", "msg, args, kwargs, want")
        cases = [
            C(
                msg="inner join",
                args=["groups", "users"],
                kwargs={"left_on": "id", "right_on": "gid"},
                want=[
                    ({"id": 1, "gname": "A"}, {"id": 10, "name": "Ax", "gid": 1}),
                    ({"id": 1, "gname": "A"}, {"id": 11, "name": "Ay", "gid": 1}),
                    ({"id": 2, "gname": "B"}, {"id": 20, "name": "Bi", "gid": 2}),
                ],
            ),
            C(
                msg="inner join2",
                args=["users", "groups"],
                kwargs={"left_on": "gid", "right_on": "id"},
                want=[
                    ({"id": 10, "name": "Ax", "gid": 1}, {"id": 1, "gname": "A"}),
                    ({"id": 11, "name": "Ay", "gid": 1}, {"id": 1, "gname": "A"}),
                    ({"id": 20, "name": "Bi", "gid": 2}, {"id": 2, "gname": "B"}),
                ],
            ),
            C(
                msg="left outer join",
                args=["groups", "users"],
                kwargs={"left_on": "id", "right_on": "gid", "how": how_left_outer_join},
                want=[
                    ({"id": 1, "gname": "A"}, {"id": 10, "name": "Ax", "gid": 1}),
                    ({"id": 1, "gname": "A"}, {"id": 11, "name": "Ay", "gid": 1}),
                    ({"id": 2, "gname": "B"}, {"id": 20, "name": "Bi", "gid": 2}),
                    ({"id": 3, "gname": "C"}, None),
                ],
            ),
            C(
                msg="right outer join",
                args=["groups", "users"],
                kwargs={
                    "left_on": "id",
                    "right_on": "gid",
                    "how": how_right_outer_join,
                },
                want=[
                    ({"id": 10, "name": "Ax", "gid": 1}, {"id": 1, "gname": "A"}),
                    ({"id": 11, "name": "Ay", "gid": 1}, {"id": 1, "gname": "A"}),
                    ({"id": 20, "name": "Bi", "gid": 2}, {"id": 2, "gname": "B"}),
                    ({"id": 40, "name": "D?", "gid": 4}, None),
                ],
            ),
            C(
                msg="full outer join",
                args=["groups", "users"],
                kwargs={"left_on": "id", "right_on": "gid", "how": how_full_outer_join},
                want=[
                    ({"id": 1, "gname": "A"}, {"id": 10, "name": "Ax", "gid": 1}),
                    ({"id": 1, "gname": "A"}, {"id": 11, "name": "Ay", "gid": 1}),
                    ({"id": 2, "gname": "B"}, {"id": 20, "name": "Bi", "gid": 2}),
                    (None, {"id": 40, "name": "D?", "gid": 4}),
                    ({"id": 3, "gname": "C"}, None),
                ],
            ),
        ]
        for c in cases:
            with self.subTest(msg=c.msg, args=c.args, kwargs=c.kwargs):
                args = [getattr(data, name) for name in c.args]
                got = self._callFUT(*args, **c.kwargs)

                self.assertTrue(
                    got == c.want, msg=_DifferenceReportText(got=got, want=c.want)
                )
                self.assertEqual(got, c.want)
                self.assertTrue(data.users == copied.users, "not modified")
                self.assertTrue(data.groups == copied.groups, "not modified")


class _DifferenceReportText:
    def __init__(self, *, got, want):
        self.got = got
        self.want = want

    def __str__(self):
        fmt = "{left:%d}\t{right:%d}" % (COLSIZE, COLSIZE)
        r = [
            "",
            fmt.format(left="want", right="got"),
            "----------------------------------------------------------------------",
        ]
        for lhs, rhs in itertools.zip_longest(self.want, self.got):
            r.append(
                fmt.format(
                    left=json.dumps(lhs, sort_keys=True),
                    right=json.dumps(rhs, sort_keys=True),
                )
            )
        return "\n".join(r)
