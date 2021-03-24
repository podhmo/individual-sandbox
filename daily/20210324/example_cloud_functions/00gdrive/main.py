from googleapiclient.discovery import build
from google.oauth2.service_account import Credentials

SCOPES = ["https://www.googleapis.com/auth/drive.metadata.readonly"]

# see:
# https://developers.google.com/drive/api/v3/quickstart/python
# https://google-auth.readthedocs.io/en/latest/user-guide.html
# https://developers.google.com/drive/api/v3/search-files


def run(*, token_file: str, credential_file: str = "credentials.json") -> None:
    creds = Credentials.from_service_account_file(token_file)
    scoped = creds.with_scopes(SCOPES)
    service = build("drive", "v3", credentials=scoped)

    # Call the Drive v3 API
    results = (
        service.files()
        .list(pageSize=10, fields="nextPageToken, files(id, name)")
        .execute()
    )
    items = results.get("files", [])

    if not items:
        print("No files found.")
    else:
        print("Files:")
        for item in items:
            print(u"{0} ({1})".format(item["name"], item["id"]))


def main():
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("token_file")
    args = parser.parse_args()
    run(**args.__dict__.copy())


if __name__ == "__main__":
    main()
