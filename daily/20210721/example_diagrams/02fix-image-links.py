import sys
from typing import Union
import pathlib
from xml.etree import ElementTree as ET


def to_base64(file_path: Union[str, pathlib.Path]) -> str:
    import base64

    with open(file_path, "rb") as image_file:
        data = base64.b64encode(image_file.read())

    return data.decode("utf-8")


def converted(filename: str) -> ET:
    t = ET.parse(filename)
    ns = {"svg": "http://www.w3.org/2000/svg", "xlink": "http://www.w3.org/1999/xlink"}

    for node in t.findall(".//svg:image[@xlink:href]", namespaces=ns):
        filepath = node.get(f"{{{ns['xlink']}}}href")
        if filepath is None:
            continue
        path = pathlib.Path(filepath)
        if not path.exists():
            print(f"{path} is not found", file=sys.stderr)

        node.set(f'{{{ns["xlink"]}}}href', "data:image/png;base64," + to_base64(path))
    return t


if __name__ == "__main__":
    t = converted("./architecture.svg")
    ET.dump(t)
