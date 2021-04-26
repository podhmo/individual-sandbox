def hello(*, name: str) -> None:
    print(f"hello {name}")


def main() -> None:
    import os
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("--name", required=True)
    args = parser.parse_args()

    params = vars(args).copy()
    fn = hello

    if bool(os.getenv("FAKE_CALL")):
        from inspect import getcallargs
        from functools import partial

        fn = partial(getcallargs, fn)
    fn(**params)


if __name__ == "__main__":
    main()
