from googleapiclient.discovery import build
from googleapiclient.http import MediaFileUpload
from google.oauth2.service_account import Credentials

SCOPES = [
    "https://www.googleapis.com/auth/drive.metadata.readonly",
    "https://www.googleapis.com/auth/drive",
]

# see:
# https://developers.google.com/drive/api/v3/quickstart/python
# https://google-auth.readthedocs.io/en/latest/user-guide.html
# https://developers.google.com/drive/api/v3/manage-uploads#simple


def run(*, token_file: str, credential_file: str = "credentials.json") -> None:
    creds = Credentials.from_service_account_file(token_file)
    scoped = creds.with_scopes(SCOPES)
    service = build("drive", "v3", credentials=scoped)

    file_metadata = {"name": "gdrive-upload-example.py"}
    target_file = __file__
    media = MediaFileUpload(target_file, mimetype="plain/text")
    file = (
        service.files()
        .create(body=file_metadata, media_body=media, fields="id")
        .execute()
    )
    print(f"File ID: {file.get('id')}")


def main():
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("token_file")
    args = parser.parse_args()
    run(**args.__dict__.copy())


if __name__ == "__main__":
    main()
