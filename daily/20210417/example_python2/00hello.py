def hello(*, name: str) -> None:
    print(f"hello {name}")


def main() -> None:
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("--name", required=True)
    args = parser.parse_args()

    params = vars(args).copy()
    hello(**params)


if __name__ == "__main__":
    main()
