import json
import os.path
import sys
from google.oauth2.credentials import Credentials
from google.auth.transport.requests import AuthorizedSession

# If modifying these scopes, delete the file token.json.
SCOPES = ["https://www.googleapis.com/auth/spreadsheets.readonly"]

# The ID and range of a sample spreadsheet.
SAMPLE_SPREADSHEET_ID = "1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgvE2upms"
SAMPLE_RANGE_NAME = "Class Data!A2:E"


def main(token_file: str, credential_file: str) -> None:
    with open(token_file, "r") as token:
        d = json.load(token)
        creds = Credentials(token=None, **d, id_token=None)
        # creds = creds.with_quota_project("")

    s = AuthorizedSession(creds)
    # res = s.get("https://www.googleapis.com/discovery/v1/apis/sheets/v4/rest")
    # print(res, json.dumps(res.json(), indent=2))

    sheet_id = SAMPLE_SPREADSHEET_ID
    range_name = SAMPLE_RANGE_NAME
    res = s.get(
        f"https://sheets.googleapis.com/v4/spreadsheets/{sheet_id}/values/{range_name}?alt=json",
        hooks={
            "response": lambda *args, **kwargs: print(args, kwargs, file=sys.stderr)
        },
    )
    print(json.dumps(res.json(), indent=2, ensure_ascii=False))


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("--token-file", required=True)
    parser.add_argument("--credential-file", required=True)
    args = parser.parse_args()
    main(
        token_file=os.path.expanduser(args.token_file),
        credential_file=os.path.expanduser(args.credential_file),
    )
