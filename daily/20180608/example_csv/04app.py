from app import Configurator

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("basefile")
    parser.add_argument("files", nargs="+")
    parser.add_argument("--on", required=True)
    args = parser.parse_args()
    import logging
    logging.basicConfig(level=logging.DEBUG)
    config = Configurator()
    config.include("./app/csv")
    config.make_app()(args)
