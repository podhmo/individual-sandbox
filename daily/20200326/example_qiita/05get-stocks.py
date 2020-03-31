import requests
import dotenv
import os
import contextlib
import sys
import re
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

        user_id = "podhmo"
        url = f"{base_url}/api/v2/users/{user_id}/stocks"

        r = []

        while url is not None:
            import time
            time.sleep(0.1)
            print(f"get url {url}", file=sys.stderr)
            response = s.get(url, params={"per_page": 100})
            response.raise_for_status()

            output = {}
            output["headers"] = dict(response.headers)
            output["response"] = {}
            output["response"]["url"] = response.url
            output["response"]["status_code"] = response.status_code
            output["response"]["content"] = response.json()
            r.append(output)

            link = response.headers.get("Link")

            if link is None:
                break

            next_url = None
            for string, kind in re.findall(r'<\s*([^;]+)>; rel="([^,]+),?', link):
                kind = kind.strip('"')
                if kind == "next":
                    next_url = string.strip("<>")
                    break

            if next_url is None:
                break
            url = next_url

        from dictknife import loading

        loading.dumpfile(r)
