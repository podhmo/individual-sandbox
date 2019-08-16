import sys
import traceback
from kamidana.driver import Driver
from kamidana.loader import TemplateLoader
from kamidana.debug._extract import extract_detail


def detect_kind(
    fs: traceback.FrameSummary,
    *,
    _cands=set(["template", "top-level template code", "template"])
) -> str:
    is_jinja2 = False
    name = fs.name
    if name in _cands or name.startswith('block "'):
        is_jinja2 = True
    elif name == "<module>" and fs.filename.endswith((".j2", ".jinja2")):
        is_jinja2 = True
    return "jinja2" if is_jinja2 else "python"


template = sys.argv[1]

try:
    loader = TemplateLoader([], [], [])

    driver = Driver(loader, format="raw")
    driver.run(template, None)
except Exception as e:
    tb = e.__traceback__
    frames = traceback.extract_tb(tb)
    for fs in frames:
        print(detect_kind(fs), fs.name)
