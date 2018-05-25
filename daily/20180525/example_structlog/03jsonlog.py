import logging
import myapp

if __name__ == "__main__":
    import jsonlog as mylog
    mylog.setup()

    log_level = logging.INFO
    logging.basicConfig(level=log_level)

    myapp.f()

    print("------reload-----------------------------")

    # reload
    import importlib
    importlib.reload(myapp)
    myapp.f()
