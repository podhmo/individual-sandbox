import json
import requests
import logging

logging.basicConfig(level=logging.CRITICAL)
requests_log = logging.getLogger("requests.packages.urllib3")
requests_log.setLevel(logging.DEBUG)
requests_log.propagate = True


def make_structure(d):
    if isinstance(d, dict):
        return {k: make_structure(v) for k, v in d.items()}
    elif isinstance(d, (list, tuple)):
        return [make_structure(x) for x in d]
    else:
        return str(type(d).__name__)

response = requests.get("https://qiita.com/api/v1/search", {"q": "python"})
data = make_structure(response.json()[:5])
# print(json.dumps(data, indent=2, ensure_ascii=False))
