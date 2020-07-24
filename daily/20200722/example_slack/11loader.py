import typing as t
import pathlib
from handofcats import as_command
from gspread.auth import (
    InstalledAppFlow,
    load_credentials,
    store_credentials,
)
from gspread.auth import DEFAULT_SCOPES


def get_credentials(
    credential_filename: str = "~/.config/sheetconf/credentials.json",
    authorized_user_filename: str = ".auth.out",
    scopes: t.Optional[t.List[str]] = DEFAULT_SCOPES,
):
    authorized_user_path = pathlib.Path(authorized_user_filename).expanduser()
    credential_path = pathlib.Path(credential_filename).expanduser()
    if not authorized_user_path.parent.exists():
        authorized_user_path.parent.mkdir(parents=True)
    if not credential_path.parent.exists():
        credential_path.parent.mkdir(parents=True)

    creds = load_credentials(filename=str(authorized_user_path))

    if not creds:
        try:
            flow = InstalledAppFlow.from_client_secrets_file(
                str(credential_path), scopes
            )
        except FileNotFoundError:
            raise  # xxx

        creds = flow.run_console()
        store_credentials(creds, filename=str(authorized_user_path))
    return creds


@as_command
def run(*, auth_file: str) -> None:
    creds = get_credentials(authorized_user_filename=auth_file)
    print(type(creds), creds)
