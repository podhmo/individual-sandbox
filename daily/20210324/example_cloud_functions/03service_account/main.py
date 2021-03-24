import json
import google.auth
from googleapiclient.discovery import build

SCOPES = ["https://www.googleapis.com/auth/drive.metadata.readonly"]
scoped, project = google.auth.default(scopes=SCOPES)


# https://cloud.google.com/functions/docs/first-python
def hello_http(request):
    """HTTP Cloud Function.
    Args:
        request (flask.Request): The request object.
        <https://flask.palletsprojects.com/en/1.1.x/api/#incoming-request-data>
    Returns:
        The response text, or any set of values that can be turned into a
        Response object using `make_response`
        <https://flask.palletsprojects.com/en/1.1.x/api/#flask.make_response>.
    """

    files = []
    error = None
    try:
        service = build("drive", "v3", credentials=scoped)
        # Call the Drive v3 API
        results = (
            service.files()
            .list(pageSize=10, fields="nextPageToken, files(id, name)")
            .execute()
        )
        items = results.get("files", [])

        files = [{"name": item["name"], "id": item["id"]} for item in (items or [])]
    except Exception as e:
        error = str(e)
    return json.dumps(
        {
            "files": files,
            "project": project,
            "error": error,
            "me": scoped.service_account_email,
        },
        indent=2,
    )
