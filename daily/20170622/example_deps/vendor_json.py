import sys
from dictknife import loading
from collections import defaultdict, OrderedDict


def collect(packages):
    store = defaultdict(list)
    for pkg in packages:
        if pkg["path"].startswith("github.com"):
            site, username, name, *suffix = pkg["path"].split("/", 3)
            store["/".join([site, username, name])].append(pkg)
        elif pkg["path"].startswith("golang.org/x"):
            site, name, subname, *suffix = pkg["path"].split("/", 3)
            store["/".join([site, name, subname])].append(pkg)
        elif pkg["path"].startswith("google.golang.org"):
            site, name, *suffix = pkg["path"].split("/", 2)
            store["/".join([site, name])].append(pkg)
        else:
            store[pkg["path"]].append(pkg)
    return store


def arrange(store):
    r = []
    for name, pkgs in store.items():
        if not all(pkgs[0]["revision"] == pkg["revision"] for pkg in pkgs):
            # fix
            copied = pkgs[:]
            for pkg in sorted(copied, key=lambda x: x["revisionTime"]):
                print(pkg["path"], pkg["revisionTime"], pkg["revision"], file=sys.stderr)
            print("----------------------------------------", file=sys.stderr)
            pkgs[0]["revision"] = next(iter(reversed(copied)))["revision"]
            # continue

        if len(pkgs) <= 1:
            d = OrderedDict()
            d["name"] = name
            d["packages"] = ["."]
            d["revision"] = pkgs[0]["revision"]
            r.append(d)
        else:
            pkgs = sorted(pkgs, key=lambda x: x["path"])
            aliases = [pkg["path"].replace(name, "") for pkg in pkgs]
            if any("." in x for x in aliases):
                site, username, name, *_ = pkgs[0]["path"].split("/")
                top_pkg_name = "/".join([site, username, name])
                aliases = [pkg["path"].replace(top_pkg_name, "") for pkg in pkgs]
                aliases.pop(0)  # .
            aliases = ["." if x == "" else x.lstrip("/") for x in aliases]
            d = OrderedDict()
            d["name"] = name
            d["packages"] = aliases
            d["revision"] = pkgs[0]["revision"]
            r.append(d)
        if r[-1]["name"].startswith("gopkg.in/"):
            r[-1]["branch"] = r[-1]["name"].rsplit(".", 1)[-1].split("/", 1)[0]
    return {"projects": r}


def run(src):
    loading.setup()
    d = loading.loadfile(src)
    store = collect(d["package"])
    r = arrange(store)
    loading.dumpfile(r, format="toml")


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("src")

    args = parser.parse_args()
    run(args.src)
