import requests
import dotenv
import os
import contextlib
import sys
from handofcats import as_command

base_url = "https://qiita.com"
# see: https://qiita.com/api/v2/docs#get-apiv2authenticated_useritems


@contextlib.contextmanager
def handle_error_response():
    try:
        yield
    except requests.HTTPError as e:
        print(e, file=sys.stderr)
        # todo: more verbose output
        print("request", e.request, file=sys.stderr)
        print("response", e.response, file=sys.stderr)


@as_command
def run():
    dotenv.load_dotenv()

    token = os.environ["TOKEN"]
    with contextlib.ExitStack() as stack:
        stack.enter_context(handle_error_response())

        s = stack.enter_context(requests.Session())
        s.headers["Authorization"] = f"Bearer {token}"

        url = f"{base_url}/api/v2/authenticated_user/items"
        response = s.get(url, params={"per_page": 100})
        response.raise_for_status()

        output = {}
        output["headers"] = dict(response.headers)
        output["response"] = {}
        output["response"]["url"] = response.url
        output["response"]["status_code"] = response.status_code
        output["response"]["content"] = response.json()

        from dictknife import loading

        loading.dumpfile(output)
