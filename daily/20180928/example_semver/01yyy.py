from semver import make_semver
print(make_semver("4.1.3.2+jenkins", loose=True).__dict__)
