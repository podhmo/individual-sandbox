package autogen

/* structure
Smartstreet
    Analysis
    Components
    Metadata
*/
type Smartstreet []struct {
	Analysis             Analysis   `json:"analysis"`
	CandidateIndex       int        `json:"candidate_index" example:"1"`
	Components           Components `json:"components"`
	DeliveryLine1        string     `json:"delivery_line_1" example:"1 S Rosedale St"`
	DeliveryPointBarcode string     `json:"delivery_point_barcode" example:"212293739011"`
	InputIndex           int        `json:"input_index" example:"0"`
	LastLine             string     `json:"last_line" example:"Baltimore MD 21229-3739"`
	Metadata             Metadata   `json:"metadata"`
}

type Analysis struct {
	Active       string `json:"active" example:"Y"`
	DpvCmra      string `json:"dpv_cmra" example:"N"`
	DpvFootnotes string `json:"dpv_footnotes" example:"AABB"`
	DpvMatchCode string `json:"dpv_match_code" example:"Y"`
	DpvVacant    string `json:"dpv_vacant" example:"N"`
}

type Components struct {
	CityName                string `json:"city_name" example:"Baltimore"`
	DeliveryPoint           string `json:"delivery_point" example:"01"`
	DeliveryPointCheckDigit string `json:"delivery_point_check_digit" example:"1"`
	Plus4Code               string `json:"plus4_code" example:"3739"`
	PrimaryNumber           string `json:"primary_number" example:"1"`
	StateAbbreviation       string `json:"state_abbreviation" example:"MD"`
	StreetName              string `json:"street_name" example:"Rosedale"`
	StreetPredirection      string `json:"street_predirection" example:"S"`
	StreetSuffix            string `json:"street_suffix" example:"St"`
	Zipcode                 string `json:"zipcode" example:"21229"`
}

type Metadata struct {
	CarrierRoute          string  `json:"carrier_route" example:"C047"`
	CongressionalDistrict string  `json:"congressional_district" example:"07"`
	CountyFips            string  `json:"county_fips" example:"24510"`
	CountyName            string  `json:"county_name" example:"Baltimore City"`
	Dst                   int     `json:"dst" example:"True"`
	ElotSequence          string  `json:"elot_sequence" example:"0064"`
	ElotSort              string  `json:"elot_sort" example:"A"`
	Latitude              float64 `json:"latitude" example:"39.2858"`
	Longitude             float64 `json:"longitude" example:"-76.66889"`
	Precision             string  `json:"precision" example:"Zip9"`
	Rdi                   string  `json:"rdi" example:"Residential"`
	RecordType            string  `json:"record_type" example:"S"`
	TimeZone              string  `json:"time_zone" example:"Eastern"`
	UtcOffset             int     `json:"utc_offset" example:"-5"`
	ZipType               string  `json:"zip_type" example:"Standard"`
}
