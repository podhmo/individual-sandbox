import logging
import gspread
from handofcats import as_command
from dictknife.loading._gsuite import get_credentials

logger = logging.getLogger(__name__)
SCOPES = [
    "https://www.googleapis.com/auth/drive",
    "https://www.googleapis.com/auth/spreadsheets",
]

# todo:
# - get sheet by sheet name
# - get or create sheet
# - iterating all cell
# - update all cell in sheet
# - take revision -> gdriveの方のAPI
# - rangeを指定できると便利なのか


def get_or_create(gc: gspread.Client, name, url=None, key=None) -> gspread.Spreadsheet:
    if url is not None:
        return gc.open_by_url(url)
    elif key is not None:
        return gc.open_by_key(key)
    else:
        try:
            return gc.open(name)
        except gspread.SpreadsheetNotFound:
            logger.info("spreadsheet %r is not found, creating it", name)
            return gc.create(name)


def get_or_create_sheet(
    wks: gspread.Spreadsheet, name, *, rows=0, cols=0
) -> gspread.Worksheet:
    try:
        return wks.worksheet(name)
    except gspread.WorksheetNotFound:
        logger.info("worksheet %r is not found in %r, creating it", name, wks)
        return wks.add_worksheet(title=name, rows=rows, cols=cols)


def to_cells(rows):
    cells = (
        [gspread.Cell(row=i, col=j, value=v) for j, v in enumerate(row, 1)]
        for i, row in enumerate(rows, 1)
    )
    return [cell for row in cells for cell in row]  # flatten


def get_client() -> gspread.Client:
    credentials = get_credentials(
        "~/.config/dictknife/credentials.json",
        cache_path="~/.config/dictknife/token4.json",
        scopes=SCOPES,
    )
    return gspread.authorize(credentials)


@as_command
def run(url: str) -> None:
    url = "https://docs.google.com/spreadsheets/d/1snkVUG7XyFtZ-CSXNoFDJuHtaTMTBgfX_BxqvqpaG0o/edit#gid=0"
    gc = get_client()
    wks = get_or_create(gc, "manytables2", url=url)

    ws = get_or_create_sheet(wks, "Group")
    cells = to_cells([("id", "name"), (1, "A"), (2, "B"), (3, "C")])
    ws.update_cells(cells, value_input_option="RAW")

    ws = get_or_create_sheet(wks, "Member")
    cells = to_cells(
        [("id", "name", "group_id"), (1, "x", 1), (2, "y", 1), (3, "z", 2)]
    )
    ws.update_cells(cells, value_input_option="RAW")
