from typing import List
from metashape.runtime import get_walker


class Data:
    data: List["Datum"]


class Datum:
    analysis: "Analysis"
    candidate_index: int
    components: "Component"
    delivery_line_1: str
    delivery_point_barcode: str
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
    delivery_point: str
    delivery_point_check_digit: str
    plus4_code: str
    primary_number: str
    state_abbreviation: str
    street_name: str
    street_predirection: str
    street_suffix: str
    zipcode: str


class Metadatum:
    carrier_route: str
    congressional_district: str
    county_fips: str
    county_name: str
    dst: bool
    elot_sequence: str
    elot_sort: str
    latitude: float
    longitude: float
    precision: str
    rdi: str
    record_type: str
    time_zone: str
    utc_offset: int
    zip_type: str


w = get_walker(Data, aggressive=True, recursive=True)
targets = list(w.walk())
for cls in targets:
    print(cls)
