import typing as t


class Schema:
    def load(self, data: t.Mapping) -> t.Tuple[t.Mapping]:
        ...
