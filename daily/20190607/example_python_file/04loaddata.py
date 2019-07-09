from importlib.util import find_spec

spec = find_spec("lib2to3")
# print(spec.loader.get_data(spec.loader.resource_path("Grammar.txt")))
print(spec.loader.path)
print(list(spec.loader.contents()))
