from typing import Union
import logging
import pathlib
from functools import lru_cache
from xml.etree import ElementTree as ET
from diagrams import Diagram
from diagrams.aws.mobile import Amplify
from diagrams.aws.mobile import APIGateway
from diagrams.aws.compute import Lambda
from diagrams.aws.security import IAM
from diagrams.aws.database import Dynamodb


logger = logging.getLogger(__name__)


@lru_cache(maxsize=128)
def to_base64(file_path: Union[str, pathlib.Path]) -> str:
    import base64

    with open(file_path, "rb") as image_file:
        data = base64.b64encode(image_file.read())

    return data.decode("utf-8")


def fix_image_links(t: ET) -> ET:
    ns = {"svg": "http://www.w3.org/2000/svg", "xlink": "http://www.w3.org/1999/xlink"}

    for node in t.findall(".//svg:image[@xlink:href]", namespaces=ns):
        filepath = node.get(f"{{{ns['xlink']}}}href")
        if filepath is None:
            continue
        path = pathlib.Path(filepath)
        if not path.exists():
            logger.info("fix image links, %s is not found", path)
            continue
        node.set(f'{{{ns["xlink"]}}}href', "data:image/png;base64," + to_base64(path))
    return t


class Extended(Diagram):
    def render(self):
        self.dot.render(format=self.outformat, view=False, quiet=True)
        if self.outformat == "svg":
            path = pathlib.Path(f"{self.filename}.{self.outformat}")
            t = ET.parse(str(path))
            fix_image_links(t)
            logger.info("fix links %s", self.filename)
            t.write(str(path))

        # TODO: support self.show


with Extended(
    "architecture",
    show=True, # not supported yet
    direction="LR",
    outformat="svg",
    filename="architecture.embed",
):
    amplify = Amplify("AWS Amplify")
    api_gateway = APIGateway("Amazon API Gateway")
    aws_lambda = Lambda("AWS lambda")
    iam = IAM("AWS Identity and Access Management")
    dynamo_db = Dynamodb("Amazon DynamoDB")

    amplify >> api_gateway >> aws_lambda >> dynamo_db
    iam >> aws_lambda

# print("@", to_base64.cache_info())
