import os.path
import logging

logging.basicConfig(level=logging.DEBUG)
SCOPES = ["https://www.googleapis.com/auth/spreadsheets.readonly"]


def oauth(scopes=SCOPES, port=0):
    from gspread.auth import (
        InstalledAppFlow,
        load_credentials,
        store_credentials,
        Client,
    )

    credential_filename = os.path.expanduser("~/.config/sheetconf/credentials.json")
    authorized_user_filename = os.path.expanduser(
        "~/.config/sheetconf/authorized_user.json"
    )
    creds = load_credentials(filename=authorized_user_filename)

    if not creds:
        flow = InstalledAppFlow.from_client_secrets_file(credential_filename, scopes)
        creds = flow.run_local_server(port=port)
        store_credentials(creds, filename=authorized_user_filename)

    client = Client(auth=creds)
    return client


gc = oauth()
sheet = gc.open_by_url(
    "https://docs.google.com/spreadsheets/d/1PgLfX5POop6QjpgjDLE9wbSWWXJYcowxRBEpxmpG8og"
)
for ws in sheet.worksheets():
    print(ws.title)
    for row in ws.get("A1:E"):
        print("\t", row)
