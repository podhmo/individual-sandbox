from importlib_metadata import distributions

for d in distributions():
    print(d.metadata["Name"], d.version)
