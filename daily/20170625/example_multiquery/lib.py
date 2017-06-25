class NameConflict(Exception):
    pass


class OrphanResourceIsFound(Exception):
    pass


class Resource:
    def __init__(self, coll, q=None):
        self.result = None
        self.mappings = {}

        self.coll = coll
        self.q = q or {}

    def in_(self, name, ids):
        if name in self.q and "$in" in self.q[name]:
            return self
        q = self.q.copy()
        q["name"] = {"$in": ids}
        return self.__class__(self.coll, q=q)

    def __iter__(self):
        if self.result is not None:
            return iter(self.result)

        self.result = []
        append = self.result.append
        for row in iter(self.coll):
            append(row)
            yield row


class MultiResource:  # dict resource ?
    def __init__(self, resources=None, relations=None):
        self.resources = resources or {}
        self.relations = relations or {}

    def __getattr__(self, k):
        try:
            return self.resources[k]
        except KeyError:
            raise AttributeError(k)

    def add(self, name, resource, force=False):
        if not force and name in self.resources:
            raise NameConflict("{} is already existed".format(name))
        new = self.__class__(self.resources.copy(), self.relations.copy())
        new.resources[name] = resource
        return new

    def remove(self, name):
        new = self.__class__(self.resources.copy(), self.relations.copy())
        new.resources.pop(name, None)
        return new

    def restrict(self):
        

    @property
    def satisfied(self):
        try:
            self.validate()
            return True
        except OrphanResourceIsFound:
            return False

    def validate(self):
        for name, r in self.resources.items():
            if not any(r in rel for rel in self.relations):
                raise OrphanResourceIsFound("{}'s relation is not found".format(name))

    def __iter__(self):
        self.validate()
        for rel in self.relations:
            rel.fetch()


class Relation:
    def __init__(self, master, slave, master_k, slave_k):
        self.mater = master
        self.slave = slave
        self.master_k = master_k
        self.slave_k = slave_k

    def fetch(self):
        pass


class ManyToOne(Relation):  # children to parent
    """master is children, slave is parent"""
    def fetch(self):
        obs = self.master
        subs = self.slave.in_(self.slave_k, [ob[self.master_k] for ob in obs])
        return {sub[self.slave_k] for sub in subs}


class OneToMany(Relation):  # parent to children
    def fetch(self):
        pass


class Merger:
    def __init__(self, structure):
        self.structure = structure
