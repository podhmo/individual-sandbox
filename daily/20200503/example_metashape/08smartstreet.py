import typing as t
from emit import emit
from prestring.go.gofmt import gofmt

IntString = int


class Data:
    data: t.List["Datum"]


class Datum:
    analysis: "Analysis"
    candidate_index: int
    components: "Component"
    delivery_line_1: str
    delivery_point_barcode: IntString
    input_index: int
    last_line: str
    metadata: "Metadatum"


class Analysis:
    active: str
    dpv_cmra: str
    dpv_footnotes: str
    dpv_match_code: str
    dpv_vacant: str


class Component:
    city_name: str
    delivery_point: IntString
    delivery_point_check_digit: IntString
    plus4_code: IntString
    primary_number: IntString
    state_abbreviation: str
    street_name: str
    street_predirection: str
    street_suffix: str
    zipcode: IntString


class Metadatum:
    carrier_route: str
    congressional_district: IntString
    county_fips: IntString
    county_name: str
    dst: bool
    elot_sequence: IntString
    elot_sort: str
    latitude: float
    longitude: float
    precision: str
    rdi: str
    record_type: str
    time_zone: str
    utc_offset: int
    zip_type: str


print(gofmt(emit([Data]), always=False))
