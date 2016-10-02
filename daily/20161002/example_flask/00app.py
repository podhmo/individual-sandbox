from flask import Flask, Config
import logging
import magicalimport
logger = logging.getLogger(__name__)


"""config by phyical file path addresse"""


class CustomConfig(Config):
    def from_pyfile(self, path):
        return self.from_object(magicalimport.import_from_physical_path(path))


class App(Flask):
    config_class = CustomConfig

# or monkey patching
# Flask.config_class = CustomConfig


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config", default=None, required=True)
    args = parser.parse_args()

    app = App(__name__)
    if args.config:
        app.config.from_pyfile(args.config)

    logging.basicConfig(
        level=app.config["LOGGING_LEVEL"].upper(),
        format=app.config["LOGGING_FORMAT"]
    )
    logger.info("running.. (port=%s)", app.config["PORT"])
    app.run(port=app.config["PORT"])
