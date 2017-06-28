from collections import ChainMap, defaultdict
import logging
logger = logging.getLogger(__name__)


class Resource:
    def __init__(self, connection):
        self.result = None
        self.mappings = {}
        self.connection = connection

    def __iter__(self):
        if self.result is not None:
            return iter(self.result)

        self.result = []

        def iterate(append):
            for row in iter(self.connection.fetch()):
                append(row)
                yield row

        return iterate(self.result.append)

    def in_(self, name, ids):
        return self.__class__(self.connection.in_(name, ids))

    def bind_one(self, name, subresource, rel):
        return bind_one(self, name, subresource, rel)

    def bind_many(self, name, subresource, rel):
        return bind_many(self, name, subresource, rel)


class SQLConnection:
    def __init__(self, connection, table, q=None):  # todo: projection
        self.connection = connection
        self.table = table
        self.q = q or []

    def fetch(self):
        # xxx:
        if self.q:
            sql = """SELECT * from {table} WHERE {q}""".format(
                table=self.table, q=" AND ".join(self.q)
            )
        else:
            sql = """SELECT * from {table}""".format(table=self.table)
        logger.debug("fetch %s", sql)
        return self.connection.execute(sql)

    def in_(self, name, ids):
        q = self.q.copy()
        # xxx:
        q.append("{} IN ({})".format(name, ", ".join(map(str, ids))))
        return self.__class__(self.connection, table=self.table, q=q)


class MongoConnection:
    def __init__(self, coll, q=None, projection=None):
        self.coll = coll
        self.q = q
        self.projection = projection

    def fetch(self):
        logger.debug("fetch %s %s", self.coll, self.q)
        return self.coll.find(self.q)

    def in_(self, name, ids):
        if name in self.q and "$in" in self.q[name]:
            return self
        q = self.q.copy()
        q[name] = {"$in": ids}
        return self.__class__(self.coll, q=q, projection=self.projection)


def bind_one(resource, name, subresource, rel):
    key, subkey = [k.split(".", 1)[-1] for k in rel.split("==")]  # xxx
    return BoundOneResource(resource, name, subresource, key=key, subkey=subkey)


def bind_many(resource, name, subresource, rel):
    key, subkey = [k.split(".", 1)[-1] for k in rel.split("==")]  # xxx
    return BoundManyResource(resource, name, subresource, key=key, subkey=subkey)


class _BoundResource:
    def __init__(self, resource, name, subresource, key, subkey):
        self.resource = resource
        self.name = name
        self.subkey = subkey
        self.key = key
        self.subresource = subresource

    @property
    def original_resource(self):
        if hasattr(self.resource, "original_resource"):
            return self.resource.original_resource
        return self.resource

    def bind_one(self, name, subresource, rel):
        return bind_one(self, name, subresource, rel)

    def bind_many(self, name, subresource, rel):
        return bind_many(self, name, subresource, rel)


class BoundOneResource(_BoundResource):
    def __iter__(self):
        gk = self.key
        sk = self.subkey
        name = self.name
        mapping = {
            sr[sk]: sr
            for sr in self.subresource.in_(sk,
                                           [row[gk] for row in self.original_resource])
        }
        for r in self.resource:
            yield ChainMap({name: mapping[r[gk]]}, r)


class BoundManyResource(_BoundResource):
    def __iter__(self):
        gk = self.key
        sk = self.subkey
        name = self.name
        mapping = defaultdict(list)
        for sr in self.subresource.in_(sk, [row[gk] for row in self.original_resource]):
            mapping[sr[sk]].append(sr)
        for r in self.resource:
            yield ChainMap({name: mapping[r[gk]]}, r)


# shorthand
class ResourceFactory:
    def mongo(self, coll, q=None, projection=None):
        return Resource(MongoConnection(coll, q=q or {}, projection=projection))

    mongo.pk = "_id"

    def sql(self, connection, table, q=None):
        return Resource(SQLConnection(connection, table, q=q))
