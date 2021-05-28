import logging
from tbvaccine import TBVaccine


class TbVaccineFormatter(logging.Formatter):
    def formatException(self, exc_info):
        return TBVaccine(isolate=True).format_exc()


def run():
    logger = logging.getLogger(__name__)
    sh = logging.StreamHandler()
    sh.setFormatter(
        TbVaccineFormatter(
            "[%(levelname)s] %(asctime)s : %(message)s", "%Y-%m-%d %H:%M:%S"
        )
    )
    logger.addHandler(sh)
    # fmt = "%(asctime)s: me=%(me)s level=%(levelname)s %(filename)s:%(lineno)s -- %(message)s"
    logging.basicConfig(level=logging.INFO)
    logger.info("x")
    try:
        1 / 0
    except:
        logger.exception("ho")


run()
