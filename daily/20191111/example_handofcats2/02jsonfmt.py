import typing as t
from handofcats import as_command


DumpFormat = t.NewType("DumpFormat", str)
DumpFormat.choices = ["json", "csv"]


@as_command
def run(*, input_format: DumpFormat, output_format: DumpFormat = "json") -> None:
    pass
