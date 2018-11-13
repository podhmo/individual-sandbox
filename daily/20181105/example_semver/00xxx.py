from semver import max_satisfying, make_semver


def partition(versions, loose=False):
    oks, ngs = [], []
    for v in versions:
        try:
            oks.append(make_semver(v, loose=loose))
        except ValueError as e:
            ngs.append((v, e))
    return oks, ngs


versions = ["1.a.1", "master", "X.2", "1.2.1", "1.3", "2.1"]
range_ = '1.3'
loose = True
oks, ngs = partition(versions, loose=loose)
print(oks)  # [<SemVer 1.2.1 >, <SemVer 1.3.0 >, <SemVer 2.1.0 >]
print(max_satisfying(oks, range_, loose=loose))  # 1.3.0
# print(make_semver("X.2", loose=True))
