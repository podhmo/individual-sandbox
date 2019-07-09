import string
from dictknife import loading
from dictknife import DictWalker


def _normalize(name):
    return name.replace("_", "-")


# todo: adhocなので整理したい
def _normalize2(name):
    # for service_name
    if name.startswith("batch-"):
        name = name[len("batch-") :]
    if name.startswith("metricsboard-"):
        name = name[len("metricsboard-"):]
    if "-proxy" in name:
        name = name.replace("-proxy", "proxy").rstrip(string.digits)
    if "mail-deliverer" in name:
        name = name.replace("mail-deliverer", "maildeliverer")

    # for exec_path
    if "_proxy" in name:
        name = name.replace("_proxy", "proxy")
    if name.startswith("./"):
        name = name[len("./") :]
    if name.startswith("metricsboard_"):
        name = name[len("metricsboard_") :]
    return name


def run(filename: str) -> str:
    d = loading.loadfile(filename)
    w = DictWalker(["vars", "exec"])
    for path, w in w.walk(d):
        # path: services/<service_name>/vars/exec
        service_name = path[-3]
        exec_path = w["exec"]
        if exec_path == "./enduser_server":
            continue
        if exec_path == "./tuner_server":
            continue

        if _normalize(_normalize2(exec_path)).startswith(
                _normalize(_normalize2(service_name))
        ):
            continue
        loading.dumpfile(
            {
                "must_include_part": _normalize(_normalize2(service_name)),
                "ref": f"#/{'/'.join(path[:-2])}",
                "exec_path": exec_path,
            },
            format="json",
        )
        print("")

def main(argv=None):
    import argparse
    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help
    parser.add_argument('filename')
    args = parser.parse_args(argv)
    run(**vars(args))


if __name__ == '__main__':
    main()
