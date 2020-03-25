import logging
import time
import typing as t

logger = logging.getLogger(__name__)


Doc = t.Dict[str, t.Any]
PK = str


class Table:
    pk: t.ClassVar[PK] = "id"

    def __init__(self, data: t.List[Doc]) -> None:
        self.data = data

    def find_all(
        self,
        *,
        where: t.Optional[t.Callable[[Doc], bool]] = None,
        in_: t.Optional[t.List[PK]] = None,
    ) -> t.List[Doc]:
        logger.info("%s find_all", self.__class__.__name__)
        time.sleep(0.1)

        data = self.data
        if in_ is not None:
            data = [d for d in data if d[self.pk] in in_]
        if where is not None:
            data = [d for d in data if where(d)]
        return data

    def find_one(
        self,
        *,
        where: t.Optional[t.Callable[[Doc], bool]] = None,
        in_: t.Optional[t.List[PK]] = None,
    ) -> t.List[Doc]:
        logger.info("%s find_one", self.__class__.__name__)
        time.sleep(0.1)

        data = self.data
        if in_ is not None:
            data = [d for d in data if d[self.pk] in in_]
        if where is not None:
            data = [d for d in data if where(d)]
        return data[0]


class AsyncTable(Table):
    def find_all(
        self,
        *,
        where: t.Optional[t.Callable[[Doc], bool]] = None,
        in_: t.Optional[t.List[PK]] = None,
    ) -> t.Awaitable[t.List[Doc]]:
        import asyncio
        from functools import partial

        return asyncio.get_event_loop().run_in_executor(
            None, partial(super().find_all, where=where, in_=in_)
        )

    def find_one(
        self,
        *,
        where: t.Optional[t.Callable[[Doc], bool]] = None,
        in_: t.Optional[t.List[PK]] = None,
    ) -> t.List[Doc]:
        import asyncio
        from functools import partial

        return asyncio.get_event_loop().run_in_executor(
            None, partial(super().find_one, where=where, in_=in_)
        )


if __name__ == "__main__":
    import unittest

    class Users(Table):
        pass

    foo = {"id": "1", "name": "foo"}
    bar = {"id": "2", "name": "bar"}
    boo = {"id": "3", "name": "boo"}
    data = [foo, bar, boo]
    users = Users(data)

    class Tests(unittest.TestCase):
        def test_find_all(self):
            got = users.find_all()
            self.assertEqual(got, data)

        def test_find_all__in(self):
            got = users.find_all(in_=["2"])
            self.assertEqual(got, [bar])

        def test_find_all__where(self):
            got = users.find_all(where=lambda d: d["name"].endswith("oo"))
            self.assertEqual(got, [foo, boo])

    unittest.main()
