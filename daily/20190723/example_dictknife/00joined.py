from dictknife.query import join, how_left_outer_join


class Joined:
    def __init__(self, left, right, *, generator):
        self.lmissing = getattr(left, "missing_value", None)
        self.rmissing = getattr(right, "missing_value", None)
        self.missing_value = (self.lmissing, self.rmissing)
        self._generator = generator

    def __iter__(self):
        for left, right in self._generator:
            if left is None:
                left = self.lmissing
            if right is None:
                right = self.rmissing
            yield (left, right)


# fmt: off
groups = [
    {"id": 1, "name": "A"},
    {"id": 2, "name": "B"},
    {"id": 3, "name": "C"},
]
# fmt: on
members = [
    {"id": 1, "name": "X", "group_id": 1},
    {"id": 2, "name": "Y", "group_id": 1},
    {"id": 3, "name": "Z", "group_id": 2},
]
skils = [
    {"id": 1, "name": "i", "member_id": 1},
    {"id": 2, "name": "j", "member_id": 1},
    {"id": 3, "name": "k", "member_id": 2},
]

rows = join(groups, members, left_on="id", right_on="group_id", how=how_left_outer_join)
rows2 = join()
for row in Joined(
    object(),
    Joined(object(), object(), []),
    generator=join(
        groups, members, left_on="id", right_on="group_id", how=how_left_outer_join
    ),
):
    print(row)
