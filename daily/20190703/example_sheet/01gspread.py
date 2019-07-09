import logging
import gspread
from handofcats import as_command
from dictknife.loading._gsuite import get_credentials

logger = logging.getLogger(__name__)
SCOPE = "https://www.googleapis.com/auth/spreadsheets"

# todo:
# - get sheet by sheet name
# - get or create sheet
# - iterating all cell
# - update all cell in sheet
# - take revision -> gdriveの方のAPI
# - rangeを指定できると便利なのか


def get_or_create_sheet(wks, name, *, rows=0, cols=0):
    try:
        return wks.worksheet(name)
    except gspread.WorksheetNotFound:
        logger.info("worksheet %r is not found in %r", name, wks)
        return wks.add_worksheet(title=name, rows=rows, cols=cols)


@as_command
def run(url: str) -> None:
    credentials = get_credentials(
        "~/.config/dictknife/credentials.json",
        cache_path="~/.config/dictknife/token2.json",
        scopes=[SCOPE],
    )

    gc = gspread.authorize(credentials)

    wks = gc.open_by_url(url)
    print(f"spreadsheet title: {wks.title}")
    for ws in wks.worksheets():
        print(f"sheet: {ws}")
        for row in ws.get_all_values():
            print(row)
        print("")

    # or
    print("@", get_or_create_sheet(wks, "heh"))
    print(wks.worksheet("Group"))
    print(wks.worksheet("Member"))
    print(wks.worksheet("Member").range("A1:C2"))
