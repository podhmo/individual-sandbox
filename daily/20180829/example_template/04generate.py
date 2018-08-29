from jinja2.environment import Template


class Object:
    def __init__(self, name, attrs, *, tracer):
        self._tracer = tracer
        self._name = name
        self._attrs = attrs

    def __getattr__(self, name):
        attr = self._attrs.get(name)
        if attr is None:
            attr = self._attrs[name] = {}
        return Object(name, attr, tracer=self._tracer)

    def __call__(self, *args, **kwargs):
        self._tracer.path[-1]["args"] = args
        self._tracer.path[-1]["kwargs"] = kwargs
        return self


class Tracer:
    def __init__(self, path):
        self.path = path


class Env:
    def __init__(self):
        self.pool = {}
        self.paths = []

    def __getattr__(self, name):
        path = [{"access": name}]
        self.paths.append(path)
        t = Tracer(path)
        attrs = self.pool.get(name)
        if attrs is None:
            attrs = self.pool[name] = {}
        ob = Object(name, attrs, tracer=t)
        return ob


# property access
t = Template("name == {{c.name}} && age > {{c.age}}")
c = Env()
t.render(c=c)
print(c.pool, c.paths)

# nested access
t = Template("name == {{c.ob.name}} && age > {{c.ob.age}}")
c = Env()
t.render(c=c)
print(c.pool, c.paths)

# method call
t = Template("name == {{c.find_friend(c.name)}} && age > {{c.ob.age}}")
c = Env()
t.render(c=c)
print(c.pool, c.paths)
