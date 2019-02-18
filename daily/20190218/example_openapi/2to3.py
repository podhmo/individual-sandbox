import logging
from dictknife.jsonknife import get_resolver
from dictknife.swaggerknife.migration import Migration


def run(*, src: str, savedir: str, log: str, dry_run: bool = False) -> None:
    logging.basicConfig(level=getattr(logging, log))

    resolver = get_resolver(src)
    # xxx: sort_keys for ambitious output (for 3.6 only?)
    with Migration(resolver, dump_options={"sort_keys": True}).migrate(
        dry_run=dry_run, keep=True, savedir=savedir
    ) as updater:
        if updater.has("swagger"):
            updater.pop("swagger")
            updater.update("openapi", "3.0.0")
        if updater.has("host"):
            url = updater.pop("host")
            if updater.has("basePath"):
                base_path = updater.pop("basePath")
                url = "{}/{}".format(url.rstrip("/"), base_path.lstrip("/"))
            description = ""

            schemas = ["http"]
            if updater.has("schemes"):
                schemas = updater.pop("schemes")
            servers = []
            for schema in schemas:
                server = updater.make_dict()
                server["url"] = "{}://{}".format(schema.rstrip(":/"), url)
                server["description"] = description
                servers.append(server)
            updater.update("/servers", servers)


def main(argv=None):
    import argparse

    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help
    parser.add_argument("--log", default="DEBUG")
    parser.add_argument("--src", required=True)
    parser.add_argument("--savedir", required=True)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args(argv)
    run(**vars(args))


if __name__ == "__main__":
    main()
