from dictknife import LooseDictWalker as Base
from dictknife.walkers import apply


class LooseDictWalker(Base):
    def __init__(self, on_container=None, on_data=None, context_factory=None):
        self.on_container = on_container
        if on_container and not callable(on_container):
            self.on_container = self.gen
        self.on_data = on_data
        if on_data and not callable(on_data):
            self.on_data = self.gen
        self.context_factory = context_factory or self.__class__.context_factory

    def gen(self, *args):
        yield args

    def on_found(self, ctx, d, k):
        if self.on_container is not None:
            yield from ctx(self, self.gen, d)
        if self.on_data is not None:
            yield from ctx(self, self.gen, d[k])

    def _walk(self, ctx, qs, d, depth):
        if depth == 0:
            return

        if not qs:
            return

        if hasattr(d, "keys"):
            for k in list(d.keys()):
                ctx.push(k)
                if apply(qs[0], k):
                    q = qs.popleft()
                    yield from self._walk(ctx, qs, d[k], depth - 1)
                    if len(qs) == 0:
                        yield from self.on_found(ctx, d, k)
                    qs.appendleft(q)
                else:
                    yield from self._walk(ctx, qs, d[k], depth)
                ctx.pop()
            return
        elif isinstance(d, (list, tuple)):
            ctx.push("[]")
            for e in d:
                yield from self._walk(ctx, qs, e, depth)
            ctx.pop()
            return
        else:
            return

d = {
    "definitions": {
        "name": {
            "type": "string",
            "description": "name of someone"
        },
        "age": {
            "type": "integer",
            "minimum": 0
        },
        "person": {
            "properties": {
                "name": {
                    "$ref": "#/definitions/name"
                },
                "age": {
                    "$ref": "#/definitions/age"
                }
            }
        }
    }
}


walker = LooseDictWalker(on_container=True, on_data=True)
for path, d in walker.walk(["$ref"], d):
    print(path, d)
