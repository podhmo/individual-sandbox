# -*- coding:utf-8 -*-
from flask import Flask
import os.path
import logging
logger = logging.getLogger(__name__)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config", default=None, required=True)
    args = parser.parse_args()

    instance_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '.'))
    app = Flask(__name__, instance_relative_config=True, instance_path=instance_path)
    if args.config:
        app.config.from_pyfile(args.config)

    logging.basicConfig(
        level=app.config["LOGGING_LEVEL"].upper(),
        format=app.config["LOGGING_FORMAT"]
    )
    logger.info("running.. (port=%s)", app.config["PORT"])
    app.run(port=app.config["PORT"])
