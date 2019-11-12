import typing_extensions as tx
from handofcats import as_command


DumpFormat = tx.Literal["json", "csv"]


@as_command
def run(*, input_format: DumpFormat, output_format: DumpFormat = "json") -> None:
    pass
