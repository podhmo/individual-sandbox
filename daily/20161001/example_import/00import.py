from importlib import machinery


def example_of_loading_module_by_physical_path():
    foo = machinery.SourceFileLoader("foo", "./foo.py").load_module()
    print("\t", foo)
    print("\t", foo.name)
    print("\t", foo._age)


def example_of_loading_module_by_physical_path2():
    machinery.SourceFileLoader("foo2", "./foo.py").load_module()
    import foo2
    print("\t", foo2.name)


print("import foo")
example_of_loading_module_by_physical_path()
print("import foo2")
example_of_loading_module_by_physical_path2()
