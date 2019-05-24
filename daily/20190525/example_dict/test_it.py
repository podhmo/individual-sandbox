import unittest
import copy
import json
import itertools
from collections import namedtuple


class Tests(unittest.TestCase):
    def _callFUT(self, *args, **kwargs):
        from merge import merge

        return merge(*args, **kwargs)

    def test_it(self):
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
                    {"id": 1, "name": "Ax", "gid": 1, "gname": "A"},
                    {"id": 1, "name": "Ay", "gid": 1, "gname": "A"},
                    {"id": 2, "name": "Bi", "gid": 2, "gname": "B"},
                ],
            ),
            C(
                msg="inner join2",
                args=["users", "groups"],
                kwargs={"left_on": "gid", "right_on": "id"},
                want=[
                    {"id": 1, "name": "Ax", "gid": 1, "gname": "A"},
                    {"id": 1, "name": "Ay", "gid": 1, "gname": "A"},
                    {"id": 2, "name": "Bi", "gid": 2, "gname": "B"},
                ],
            ),
            C(
                msg="left outer join",
                args=["users", "groups"],
                kwargs={"left_on": "gid", "right_on": "id", "how": "left"},
                want=[
                    {"id": 1, "name": "Ax", "gid": 1, "gname": "A"},
                    {"id": 1, "name": "Ay", "gid": 1, "gname": "A"},
                    {"id": 2, "name": "Bi", "gid": 2, "gname": "B"},
                    {"id": 40, "name": "D?", "gid": 4, "gname": None},
                ],
            ),
            C(
                msg="right outer join",
                args=["users", "groups"],
                kwargs={"left_on": "gid", "right_on": "id", "how": "right"},
                want=[
                    {"id": 10, "name": "Ax", "gid": 1, "gname": "A"},
                    {"id": 11, "name": "Ay", "gid": 1, "gname": "A"},
                    {"id": 20, "name": "Bi", "gid": 2, "gname": "B"},
                    {"id": 3, "name": None, "gid": None, "gname": "C"},
                ],
            ),
            C(
                msg="full outer join",
                args=["users", "groups"],
                kwargs={"left_on": "gid", "right_on": "id", "how": "outer"},
                want=[
                    {"id": 1, "name": "Ax", "gid": 1, "gname": "A"},
                    {"id": 1, "name": "Ay", "gid": 1, "gname": "A"},
                    {"id": 2, "name": "Bi", "gid": 2, "gname": "B"},
                    {"id": 40, "name": "D?", "gid": 4, "gname": None},
                    {"id": 3, "name": None, "gid": None, "gname": "C"},
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

                self.assertTrue(data.users == copied.users, "not modified")
                self.assertTrue(data.groups == copied.groups, "not modified")


class _DifferenceReportText:
    def __init__(self, *, got, want):
        self.got = got
        self.want = want

    def __str__(self):
        r = [
            "",
            f"{'want':50}\t{'got':50}",
            "----------------------------------------------------------------------",
        ]
        for lhs, rhs in itertools.zip_longest(self.want, self.got):
            r.append(
                f"{json.dumps(lhs, sort_keys=True):50}\t{json.dumps(rhs, sort_keys=True):50}"
            )
        return "\n".join(r)
