import semver

cands = ["1.2.3-alpha.1"]
print(semver.max_satisfying(cands, "~1.2.3"))
print(semver.max_satisfying(cands, "~1.2.3", True))
print(semver.max_satisfying(cands, "~1.2.3-"))
print(semver.max_satisfying(cands, "~1.2.3-", True)) # 1.2.3-a.111 (only this)
print(semver.max_satisfying(cands, "~1.2.3-*"))
print(semver.max_satisfying(cands, "~1.2.3-*", True))
