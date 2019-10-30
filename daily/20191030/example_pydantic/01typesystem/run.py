import typesystem
import handofcats
from config import Config


@handofcats.as_command
def run(*, config: str) -> None:
    import sys

    with open(config) as rf:
        text = rf.read()
    try:
        c = typesystem.validate_json(text, validator=Config)
    except typesystem.ValidationError as e:
        for message in e.messages():
            line_no = message.start_position.line_no
            column_no = message.start_position.column_no
            print(f"Error {message.text!r} at line {line_no}, column {column_no}.")
        sys.exit(1)
    print(c)
