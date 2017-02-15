import magicalimport


class Driver:
    def run(self, target):
        print("hello {target}".format(target=target))


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--target", default="world")
    parser.add_argument("--driver", default="{}:Driver".format(__name__))
    args = parser.parse_args()

    driver_class = magicalimport.import_symbol(args.driver)
    driver = driver_class()
    driver.run(args.target)


if __name__ == "__main__":
    main()

