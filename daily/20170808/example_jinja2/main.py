from powerset import powerset


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("values", nargs="+")
    args = parser.parse_args()
    print(powerset(args.values))


if __name__ == "__main__":
    main()
