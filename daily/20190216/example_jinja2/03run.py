import jinja2
from jinja2.sandbox import SandboxedEnvironment

def _exception_handler(self, tb):
    import pdb; pdb.set_trace()



# class E(jinja2.Environment):
class E(SandboxedEnvironment):
    pass
# exception_handler = _exception_handler


env = E(loader=jinja2.FileSystemLoader("."))
t = env.get_template("02include-ng/zero.div.j2")
# t = env.get_template("02include-ng/main.j2")
print(t.render())
