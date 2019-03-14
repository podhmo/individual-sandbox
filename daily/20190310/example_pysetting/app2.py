import sys
from importlib.util import spec_from_file_location
from importlib.util import module_from_spec


def load_module_by_path(module_id, path):
    spec = spec_from_file_location(module_id, path)
    module = module_from_spec(spec)
    spec.loader.exec_module(module)
    sys.modules[module_id] = module
    return module


def run(*, setting: str) -> None:
    setting = load_module_by_path("setting", setting)
    setting.LOGGER.info("hello")


def main(argv=None):
    import argparse

    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help
    parser.add_argument("--setting", required=True)
    args = parser.parse_args(argv)
    run(**vars(args))


if __name__ == "__main__":
    main()
