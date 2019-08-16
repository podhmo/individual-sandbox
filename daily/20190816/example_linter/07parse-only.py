import subprocess
import sys
import logging
from dictknife.jsonknife import bundle
from dictknife import loading
logger = logging.getLogger(__name__)


def main():
    filename = sys.argv[1]
    data = bundle(filename, flavor="openapiv3")
    loading.dumpfile(data)


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, format="%(message)s")
    main()
