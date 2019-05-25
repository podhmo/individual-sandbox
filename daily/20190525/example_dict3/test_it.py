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
                    ({"id": 3, "gname": "C"}, None),
                    (None, {"id": 40, "name": "D?", "gid": 4}),
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

    def test_multi_keys(self):
        # no name change (e.g. users.name, skills.name)
        # (id is conflicted, on merge, skipped)
        class data:
            classes = [
                {"id": 1, "year": "1", "name": "A"},
                {"id": 2, "year": "1", "name": "B"},
                {"id": 3, "year": "1", "name": "C"},
                {"id": 4, "year": "2", "name": "A"},
                {"id": 5, "year": "2", "name": "B"},
            ]
            students = [
                {"id": 1, "year": "1", "class": "A", "cid": 1, "name": "foo"},
                {"id": 2, "year": "1", "class": "A", "cid": 1, "name": "bar"},
                {"id": 3, "year": "1", "class": "B", "cid": 2, "name": "boo"},
                {"id": 4, "year": "1", "class": "C", "cid": 3, "name": "yoo"},
            ]

        class copied:
            classes = copy.deepcopy(data.classes)
            students = copy.deepcopy(data.students)

        C = namedtuple("C", "msg, args, kwargs, want")
        cases = [
            C(
                msg="inner join",
                args=[data.classes, data.students],
                kwargs={"left_on": "id", "right_on": "cid"},
                want=[
                    (
                        {"id": 1, "year": "1", "name": "A"},
                        {"id": 1, "year": "1", "class": "A", "cid": 1, "name": "foo"},
                    ),
                    (
                        {"id": 1, "year": "1", "name": "A"},
                        {"id": 2, "year": "1", "class": "A", "cid": 1, "name": "bar"},
                    ),
                    (
                        {"id": 2, "year": "1", "name": "B"},
                        {"id": 3, "year": "1", "class": "B", "cid": 2, "name": "boo"},
                    ),
                    (
                        {"id": 3, "year": "1", "name": "C"},
                        {"id": 4, "year": "1", "class": "C", "cid": 3, "name": "yoo"},
                    ),
                ],
            ),
            C(
                msg="inner join with multi keys",
                args=[data.classes, data.students],
                kwargs={"left_on": ("year", "name"), "right_on": ("year", "class")},
                want=[
                    (
                        {"id": 1, "year": "1", "name": "A"},
                        {"id": 1, "year": "1", "class": "A", "cid": 1, "name": "foo"},
                    ),
                    (
                        {"id": 1, "year": "1", "name": "A"},
                        {"id": 2, "year": "1", "class": "A", "cid": 1, "name": "bar"},
                    ),
                    (
                        {"id": 2, "year": "1", "name": "B"},
                        {"id": 3, "year": "1", "class": "B", "cid": 2, "name": "boo"},
                    ),
                    (
                        {"id": 3, "year": "1", "name": "C"},
                        {"id": 4, "year": "1", "class": "C", "cid": 3, "name": "yoo"},
                    ),
                ],
            ),
            C(
                msg="inner join with multi keys2",
                args=[data.students, data.classes],
                kwargs={"left_on": ("year", "class"), "right_on": ("year", "name")},
                want=[
                    (
                        {"id": 1, "year": "1", "class": "A", "cid": 1, "name": "foo"},
                        {"id": 1, "year": "1", "name": "A"},
                    ),
                    (
                        {"id": 2, "year": "1", "class": "A", "cid": 1, "name": "bar"},
                        {"id": 1, "year": "1", "name": "A"},
                    ),
                    (
                        {"id": 3, "year": "1", "class": "B", "cid": 2, "name": "boo"},
                        {"id": 2, "year": "1", "name": "B"},
                    ),
                    (
                        {"id": 4, "year": "1", "class": "C", "cid": 3, "name": "yoo"},
                        {"id": 3, "year": "1", "name": "C"},
                    ),
                ],
            ),
        ]
        for c in cases:
            with self.subTest(msg=c.msg, kwargs=c.kwargs):
                got = self._callFUT(*c.args, **c.kwargs)

                self.assertTrue(
                    got == c.want, msg=_DifferenceReportText(got=got, want=c.want)
                )

                self.assertTrue(data.students == copied.students, "not modified")
                self.assertTrue(data.classes == copied.classes, "not modified")


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
