# pyramid 1ファイルであれこれしたくなった。

bottleとかに負けるのなんか悔しいし。
toybox/simpleapiとか作っていた。

- https://github.com/podhmo/toybox

現状はここまで短くなる。

- [./example_web/02toybox/app.py]

どうせなのでpserve,proutesなどは使えるようにしたいなー。

# pyramid routeとviewを定義するのが面倒な場合

```python
def add_simple_view(config, view, path):
    def callback():
        route_name = view.__qualname__
        config.add_route(route_name, path)
        config.add_view(view, route_name=route_name)
    discriminator = ('add_simple_view', path)
    config.action(discriminator, callback)


def includeme(config):
    config.add_directive("add_simple_view", add_simple_view)
```

venusian対応

```python
class simple_view(object):
    def __init__(self, path):
        self.path = path

    def register(self, scanner, name, wrapped):
        scanner.config.add_simple_view(wrapped, self.path)

    def __call__(self, wrapped):
        venusian.attach(wrapped, self.register)
        return wrapped
```

# pyramid だいたいのservice作る場合

```python
    def get_routes_mapper(self):
        """ Return the :term:`routes mapper` object associated with
        this configurator's :term:`registry`."""
        mapper = self.registry.queryUtility(IRoutesMapper)
        if mapper is None:
            mapper = RoutesMapper()
            self.registry.registerUtility(mapper, IRoutesMapper)
        return mapper
```
