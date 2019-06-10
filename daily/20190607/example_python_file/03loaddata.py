from importlib.util import find_spec

spec = find_spec("lib2to3")
print(spec.loader.path)
print(spec.loader.resource_path("Grammar.txt"))
print(spec.loader.open_resource("Grammar.txt").read())

