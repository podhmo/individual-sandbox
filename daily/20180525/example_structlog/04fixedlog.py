import logging
import fixedlog
import myapp2
logger = fixedlog.get_logger(__name__)

if __name__ == "__main__":
    fixedlog.setup()

    log_level = logging.INFO
    logging.basicConfig(level=log_level)

    logger.info("start.")
    myapp2.f()

    print("------reload-----------------------------")

    # reload
    import importlib
    importlib.reload(myapp2)
    myapp2.f()

    logger.info("end.")
