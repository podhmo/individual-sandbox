def run(ouput_name: str, *, output_format: str) -> None:
    pass

def main(argv=None):
    import argparse
    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help
    parser.add_argument('ouput_name')
    parser.add_argument('--output-format', required=True)
    args = parser.parse_args(argv)
    run(**vars(args))


if __name__ == '__main__':
    main()
