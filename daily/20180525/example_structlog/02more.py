import logging
import myapp

if __name__ == "__main__":
    import mylog
    mylog.setup()

    log_level = logging.INFO
    logging.basicConfig(level=log_level)
    mylog.set_loglevel(log_level)

    myapp.f()

    print("------reload-----------------------------")

    # reload
    import importlib
    importlib.reload(myapp)
    myapp.f()
