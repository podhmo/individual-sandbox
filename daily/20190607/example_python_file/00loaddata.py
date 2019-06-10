import pkg_resources

print(pkg_resources.resource_string("lib2to3", "Grammar.txt").decode("utf-8"))
