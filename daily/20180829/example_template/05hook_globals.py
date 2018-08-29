from pprint import pprint
from tracelib import (  # ./tracelib.py
    Object,
    Tracer,
    Env,
)


class App:
    def make_object(self, name):
        return Object(name, {}, tracer=Tracer([]))

    def make_env(self):
        return Env(tracer=Tracer([]))

    def run(self, code, local_env):
        if not code.startswith(("f'", 'f"')):
            code = f"f{code!r}"

        env = self.make_env()
        exec(code, {"__builtins__": env}, local_env)
        return env


code = """{c["prefix"]}-{c["dbname"]}"""
# code = """{c.x.label} + {c.y.label} = {sum([c.x.value, c.y.value])}"""
app = App()
env = app.run(code, {})
pprint(vars(env))
