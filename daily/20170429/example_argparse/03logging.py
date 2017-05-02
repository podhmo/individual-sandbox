import argparse
import logging

parser = argparse.ArgumentParser()
loglevels = list(logging._nameToLevel.keys())  # _　prefixのものなのでお行儀は良くない
parser.add_argument("--loglevel", choices=loglevels, default="INFO")

args = parser.parse_args()
logging.basicConfig(level=args.loglevel)
logger = logging.getLogger("log")

logger.debug("hmm..")
logger.info("hmm")

