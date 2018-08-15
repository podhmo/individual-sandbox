

def run(*, x: int, y: int):
    print(f"{x} + {y} = {x + y}")



def main():
    import argparse
    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help
    parser.add_argument('-x', required=True)
    parser.add_argument('-y', required=True)
    args = parser.parse_args()
    run(**vars(args))


if __name__ == '__main__':
    main()
