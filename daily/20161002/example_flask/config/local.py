import magicalimport
base = magicalimport.import_from_physical_path("base.py", as_="base", here=__file__)

LOGGING_LEVEL = "info"
LOGGING_FORMAT = base.LTSV_LOGGING_FORMAT


PORT = 8888
