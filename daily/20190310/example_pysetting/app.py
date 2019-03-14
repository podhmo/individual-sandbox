import runpy


def run(*, setting: str) -> None:
    setting = runpy.run_path(setting)
    setting["LOGGER"].info("hello")


def main(argv=None):
    import argparse

    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help
    parser.add_argument("--setting", required=True)
    args = parser.parse_args(argv)
    run(**vars(args))


if __name__ == "__main__":
    main()
