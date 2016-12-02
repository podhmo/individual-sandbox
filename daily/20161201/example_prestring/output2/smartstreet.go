package autogen

/* structure
Smartstreet
    Analysis
    Components
    Metadata
*/
type Smartstreet []struct {
	Analysis             Analysis   `json:"analysis"`
	CandidateIndex       int        `json:"candidate_index"`
	Components           Components `json:"components"`
	DeliveryLine1        string     `json:"delivery_line_1"`
	DeliveryPointBarcode string     `json:"delivery_point_barcode"`
	InputIndex           int        `json:"input_index"`
	LastLine             string     `json:"last_line"`
	Metadata             Metadata   `json:"metadata"`
}

type Analysis struct {
	Active       string `json:"active"`
	DpvCmra      string `json:"dpv_cmra"`
	DpvFootnotes string `json:"dpv_footnotes"`
	DpvMatchCode string `json:"dpv_match_code"`
	DpvVacant    string `json:"dpv_vacant"`
}

type Components struct {
	CityName                string `json:"city_name"`
	DeliveryPoint           string `json:"delivery_point"`
	DeliveryPointCheckDigit string `json:"delivery_point_check_digit"`
	Plus4Code               string `json:"plus4_code"`
	PrimaryNumber           string `json:"primary_number"`
	StateAbbreviation       string `json:"state_abbreviation"`
	StreetName              string `json:"street_name"`
	StreetPredirection      string `json:"street_predirection"`
	StreetSuffix            string `json:"street_suffix"`
	Zipcode                 string `json:"zipcode"`
}

type Metadata struct {
	CarrierRoute          string  `json:"carrier_route"`
	CongressionalDistrict string  `json:"congressional_district"`
	CountyFips            string  `json:"county_fips"`
	CountyName            string  `json:"county_name"`
	Dst                   int     `json:"dst"`
	ElotSequence          string  `json:"elot_sequence"`
	ElotSort              string  `json:"elot_sort"`
	Latitude              float64 `json:"latitude"`
	Longitude             float64 `json:"longitude"`
	Precision             string  `json:"precision"`
	Rdi                   string  `json:"rdi"`
	RecordType            string  `json:"record_type"`
	TimeZone              string  `json:"time_zone"`
	UtcOffset             int     `json:"utc_offset"`
	ZipType               string  `json:"zip_type"`
}
