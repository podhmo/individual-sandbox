import pickle
import os.path
from googleapiclient.discovery import build
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request

# https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets.values/batchGet

# If modifying these scopes, delete the file token.pickle.
SCOPES = ["https://www.googleapis.com/auth/spreadsheets.readonly"]

url = "https://docs.google.com/spreadsheets/d/1PgLfX5POop6QjpgjDLE9wbSWWXJYcowxRBEpxmpG8og"
# The ID and range of a sample spreadsheet.
SAMPLE_SPREADSHEET_ID = "1PgLfX5POop6QjpgjDLE9wbSWWXJYcowxRBEpxmpG8og"
CREDENTIAL_FILE = os.path.expanduser("~/.config/sheetconf/credentials.json")


def main():
    """Shows basic usage of the Sheets API.
    Prints values from a sample spreadsheet.
    """
    creds = None
    # The file token.pickle stores the user's access and refresh tokens, and is
    # created automatically when the authorization flow completes for the first
    # time.
    if os.path.exists("token.pickle"):
        with open("token.pickle", "rb") as token:
            creds = pickle.load(token)
    # If there are no (valid) credentials available, let the user log in.
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(CREDENTIAL_FILE, SCOPES)
            creds = flow.run_local_server(port=0)
        # Save the credentials for the next run
        with open("token.pickle", "wb") as token:
            pickle.dump(creds, token)

    service = build("sheets", "v4", credentials=creds)

    sheet = service.spreadsheets()

    ranges = ["xxx!A1:D", "yyy!A1:D"]
    # 存在しないworksheetを指定するとエラーになる
    # ranges = ["xxx!A1:D", "yyy!A1:D", "zzz!A1:D"]

    sheet_result = sheet.get(spreadsheetId=SAMPLE_SPREADSHEET_ID).execute()
    print("----------------------------------------")
    print([sheet["properties"]["title"] for sheet in sheet_result["sheets"]])
    print("----------------------------------------")

    result = (
        sheet.values()
        .batchGet(spreadsheetId=SAMPLE_SPREADSHEET_ID, ranges=ranges,)
        .execute()
    )

    for valueRanges in result.get("valueRanges") or []:
        section = valueRanges.get("range", "-")
        values = valueRanges.get("values", [])
        print(section)
        for row in values:
            # Print columns A and E, which correspond to indices 0 and 4.
            print("\t", ", ".join(row))


# e.g.
# {
#   "spreadsheetId": "1PgLfX5POop6QjpgjDLE9wbSWWXJYcowxRBEpxmpG8og",
#   "valueRanges": [
#     {
#       "majorDimension": "ROWS",
#       "range": "xxx!A1:D1000",
#       "values": [
#         [
#           "name",
#           "value",
#           "value_type",
#           "description"
#         ],
#         [
#           "name",
#           "xxx",
#           "str"
#         ],
#         [
#           "token",
#           "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
#           "str"
#         ]
#       ]
#     },
#     {
#       "majorDimension": "ROWS",
#       "range": "yyy!A1:D1000",
#       "values": [
#         [
#           "name",
#           "value",
#           "value_type",
#           "description"
#         ],
#         [
#           "name",
#           "xxx",
#           "str"
#         ],
#         [
#           "token",
#           "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
#           "str"
#         ]
#       ]
#     }
#   ]
# }


if __name__ == "__main__":
    main()
